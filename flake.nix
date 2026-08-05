{
  description = "selfflowy — self-hosted outliner (#lang selfflowy + CLI)";

  # Sources (nixpkgs + Racket package git revs) are pinned via npins.
  # See npins/sources.json; update with: npins update / npins add ...
  outputs = { self }:
    let
      sources = import ./npins;

      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map (system: {
        name = system;
        value = f {
          inherit system;
          pkgs = import sources.nixpkgs {
            inherit system;
            config = { };
            overlays = [ ];
          };
        };
      }) systems);

      # Racket packages from npins. Monorepos need a subdir; others install at root.
      # Install order is bottom-up. markdown and selfflowy use --deps force because
      # catalog package names (parsack, gregor) differ from the lib package dirs.
      racketPkgs = [
        { name = "memoize-lib"; pin = "memoize"; subdir = "memoize-lib"; }
        { name = "parsack-lib"; pin = "parsack"; subdir = "parsack-lib"; }
        { name = "threading-lib"; pin = "threading"; subdir = "threading-lib"; }
        { name = "cldr-core"; pin = "cldr-core"; subdir = null; }
        { name = "cldr-bcp47"; pin = "cldr-bcp47"; subdir = null; }
        { name = "cldr-dates-modern"; pin = "cldr-dates-modern"; subdir = null; }
        { name = "cldr-localenames-modern"; pin = "cldr-localenames-modern"; subdir = null; }
        { name = "cldr-numbers-modern"; pin = "cldr-numbers-modern"; subdir = null; }
        { name = "tzinfo"; pin = "tzinfo"; subdir = null; }
        { name = "gregor-lib"; pin = "gregor"; subdir = "gregor-lib"; }
        { name = "markdown"; pin = "markdown"; subdir = null; }
      ];
    in
    {
      devShells = forAllSystems ({ pkgs, system }: {
        default = pkgs.mkShell {
          packages = [
            pkgs.racket
            pkgs.just
            pkgs.watchexec
            pkgs.tzdata
            pkgs.npins
            self.packages.${system}.acp-agent
          ];
          shellHook = ''
            export PLTUSERHOME="''${PLTUSERHOME:-$PWD/.plt-user}"
            mkdir -p "$PLTUSERHOME"
            if [ -d "${pkgs.tzdata}/share/zoneinfo" ]; then
              export TZDIR="${pkgs.tzdata}/share/zoneinfo"
            fi
            # `serve` refuses to start without an ACP agent; hand it the
            # bundled one so `just serve` works out of the box. Set the var
            # yourself to point at a different agent.
            export SELFFLOWY_ACP_AGENT="''${SELFFLOWY_ACP_AGENT:-${self.packages.${system}.acp-agent}/bin/claude-agent-acp}"
          '';
        };
      });

      packages = forAllSystems ({ pkgs, system }:
        let
          # Stage each npins source into $out/<name> for raco pkg install --copy.
          # Writable copies so we can strip markdown test modules that need
          # optional build-deps (sexp-diff, redex) not required at runtime.
          racketDeps = pkgs.stdenvNoCC.mkDerivation {
            name = "selfflowy-racket-deps";
            dontUnpack = true;
            # npins sources are fixed-output store paths; string context pulls them in.
            buildCommand = ''
              mkdir -p $out
              ${pkgs.lib.concatMapStringsSep "\n" (p:
                let src = sources.${p.pin};
                in ''
                  echo "staging ${p.name} from ${p.pin}"
                  ${if p.subdir == null then ''
                    cp -a "${src}" "$out/${p.name}"
                  '' else ''
                    cp -a "${src}/${p.subdir}" "$out/${p.name}"
                  ''}
                  chmod -R u+w "$out/${p.name}"
                '') racketPkgs}

              # markdown ships test modules that require sexp-diff/redex at compile
              # time; strip them so offline install only needs runtime deps.
              if [ -d "$out/markdown/markdown" ]; then
                rm -f "$out/markdown/markdown/"*test*.rkt \
                      "$out/markdown/markdown/suite-test.rkt" \
                      "$out/markdown/markdown/perf-test.rkt" \
                      "$out/markdown/markdown/random-test.rkt" \
                      "$out/markdown/markdown/redex-test.rkt" \
                      "$out/markdown/markdown/example.rkt"
                rm -rf "$out/markdown/markdown/test" \
                       "$out/markdown/MarkdownTest_1.0.3" \
                       "$out/markdown/markdown/doc"
              fi
            '';
          };

          # The ACP agent `selfflowy serve` spawns. npm is the only channel the
          # adapter ships through, so it gets the npins treatment: a committed
          # lockfile in acp/, one fixed-output derivation (npmDepsHash) for the
          # tarballs, nothing fetched at build time and no npx at run time.
          # Regenerate after bumping acp/package.json:
          #   cd acp && npm install --package-lock-only --ignore-scripts
          #   set npmDepsHash to lib.fakeHash, build, paste the hash it prints.
          # The lockfile names a prebuilt `claude` for every platform npm knows
          # about, so the deps FOD is ~640M and the hash is the same on every
          # system; `npm ci` then keeps only the host's copy (~300M installed).
          acpAgent = pkgs.buildNpmPackage {
            pname = "selfflowy-acp-agent";
            version = "0.64.2"; # tracks @agentclientprotocol/claude-agent-acp
            src = ./acp;
            npmDepsHash = "sha256-Dk6VfZ7VPXtPWejwzAR4FUKJkyxgL2QrD7LWnnsH25U=";

            # acp/ is a shim around one dependency: nothing to compile, and no
            # package in the tree has an install script to run.
            dontNpmBuild = true;
            npmFlags = [ "--ignore-scripts" ];

            # The SDK ships `claude` as a bun-compiled executable. Both the
            # stripper and any RPATH rewrite move offsets the bun runtime reads
            # back out of its own file, and it segfaults; only the interpreter
            # may be touched (see postInstall).
            dontStrip = true;
            dontPatchELF = true;

            nativeBuildInputs = [ pkgs.makeWrapper ]
              ++ pkgs.lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.patchelf;

            postInstall =
              let
                # npm's own platform naming: linux-x64, darwin-arm64, ...
                nodeArch = "${pkgs.stdenv.hostPlatform.node.platform}-${pkgs.stdenv.hostPlatform.node.arch}";
                mods = "$out/lib/node_modules/selfflowy-acp/node_modules";
              in
              ''
                entry="${mods}/@agentclientprotocol/claude-agent-acp/dist/index.js"
                test -f "$entry"
                claude="${mods}/@anthropic-ai/claude-agent-sdk-${nodeArch}/claude"
                test -x "$claude"
              '' + pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
                patchelf --set-interpreter \
                  "$(cat "${pkgs.stdenv.cc}/nix-support/dynamic-linker")" "$claude"
              '' + ''
                # Node is pinned and so is the CLI the SDK drives (the adapter
                # reads CLAUDE_CODE_EXECUTABLE before it goes looking); nothing
                # here resolves off PATH. The rest of the env is what nixpkgs'
                # claude-code sets: no self-update (this closure is immutable),
                # and the ripgrep buried in the bun archive cannot be patched,
                # so hand it the one from the store.
                makeWrapper ${pkgs.nodejs}/bin/node "$out/bin/claude-agent-acp" \
                  --add-flags "$entry" \
                  --set-default CLAUDE_CODE_EXECUTABLE "$claude" \
                  --set DISABLE_AUTOUPDATER 1 \
                  --set DISABLE_INSTALLATION_CHECKS 1 \
                  --set USE_BUILTIN_RIPGREP 0 \
                  --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.ripgrep pkgs.procps ]}"
              '';

            # No meta.license on purpose: the adapter is Apache-2.0 but the
            # claude binary it drives ships under Anthropic's commercial terms,
            # and declaring that unfree would make `nix build` demand
            # allowUnfree from every consumer of this flake.
            meta = with pkgs.lib; {
              description = "Claude Code ACP adapter, pinned for selfflowy serve";
              mainProgram = "claude-agent-acp";
              platforms = platforms.unix;
            };
          };

          selfflowy = pkgs.stdenv.mkDerivation {
            pname = "selfflowy";
            version = "0.1.0";
            src = ./.;
            nativeBuildInputs = [ pkgs.racket pkgs.makeWrapper ];
            buildInputs = [ pkgs.tzdata ];

            # Zoneinfo for gregor/tzinfo during build (sandbox has no /usr/share).
            TZDIR = "${pkgs.tzdata}/share/zoneinfo";

            buildPhase = ''
              export PLTUSERHOME="$TMPDIR/plt-user"
              mkdir -p "$PLTUSERHOME"
              export TZDIR="${pkgs.tzdata}/share/zoneinfo"

              # tzinfo searches relative cwd paths and PLTUSERHOME share dirs.
              mkdir -p tzdata
              ln -sfn "${pkgs.tzdata}/share/zoneinfo" tzdata/zoneinfo
              mkdir -p "$PLTUSERHOME/.local/share/racket/9.2/share/tzdata"
              ln -sfn "${pkgs.tzdata}/share/zoneinfo" \
                "$PLTUSERHOME/.local/share/racket/9.2/share/tzdata/zoneinfo"

              cp -a "$src/selfflowy" ./selfflowy-pkg
              chmod -R u+w ./selfflowy-pkg

              # Offline install of npins-vendored deps (order matters).
              # --deps force: markdown wants package name "parsack"; we ship
              # parsack-lib. selfflowy wants "gregor"; we ship gregor-lib.
              ${pkgs.lib.concatMapStringsSep "\n" (p: ''
                echo "raco pkg install ${p.name}"
                raco pkg install --copy --no-docs --deps force --batch "${racketDeps}/${p.name}"
              '') racketPkgs}

              raco pkg install --no-docs --deps force --link ./selfflowy-pkg

              raco exe ++lang selfflowy -o selfflowy-bin \
                "$(racket -e '(display (path->string (collection-file-path "cli.rkt" "selfflowy")))')"
              raco distribute dist selfflowy-bin
            '';

            installPhase = ''
              mkdir -p $out
              cp -a dist/. $out/
              test -x $out/bin/selfflowy-bin
              # Wrap with TZDIR so gregor finds zoneinfo outside /usr/share
              mv $out/bin/selfflowy-bin $out/bin/.selfflowy-wrapped
              makeWrapper $out/bin/.selfflowy-wrapped $out/bin/selfflowy \
                --set TZDIR "${pkgs.tzdata}/share/zoneinfo" \
                --prefix PATH : "${pkgs.tzdata}/bin"
              mkdir -p $out/share/tzdata
              ln -sfn "${pkgs.tzdata}/share/zoneinfo" $out/share/tzdata/zoneinfo
            '';

            meta = with pkgs.lib; {
              description = "selfflowy CLI — validate and render #lang selfflowy outlines";
              mainProgram = "selfflowy";
              license = licenses.agpl3Plus;
            };
          };
        in
        {
          default = selfflowy;
          inherit selfflowy;
          racket-deps = racketDeps;
          acp-agent = acpAgent;
        });

      # `nix run` starts the web view; `nix run .#cli -- check ...` is the CLI.
      # A one-line exec wrapper around the built binary is the boring option:
      # no second closure, nothing to keep in sync, works offline.
      apps = forAllSystems ({ pkgs, system }:
        let
          cli = "${self.packages.${system}.selfflowy}/bin/selfflowy";
          # serve wants an ACP agent and will not start without one, so the app
          # hands it the bundled adapter. Only serve: the bare CLI stays lean
          # and never drags the node closure in. An exported var wins.
          serve = pkgs.writeShellScriptBin "selfflowy-serve" ''
            export SELFFLOWY_ACP_AGENT="''${SELFFLOWY_ACP_AGENT:-${self.packages.${system}.acp-agent}/bin/claude-agent-acp}"
            exec ${cli} serve "$@"
          '';
          serveApp = {
            type = "app";
            program = "${serve}/bin/selfflowy-serve";
          };
        in
        {
          default = serveApp;
          serve = serveApp;
          cli = {
            type = "app";
            program = cli;
          };
        });

      checks = forAllSystems ({ pkgs, system }: {
        build = self.packages.${system}.selfflowy;
        smoke = pkgs.runCommand "selfflowy-smoke"
          {
            nativeBuildInputs = [
              self.packages.${system}.selfflowy
              pkgs.racket
              pkgs.curl
            ];
          }
          ''
            export TZDIR="${pkgs.tzdata}/share/zoneinfo"
            selfflowy check ${./examples/Example.rkt}

            # Parse the JSON; never grep it (key order is not a contract).
            selfflowy tree ${./examples/Example.rkt} > tree.json
            racket -e '(require json)
                       (define j (call-with-input-file "tree.json" read-json))
                       (unless (and (= 1 (hash-ref j (quote version)))
                                    (string? (hash-ref j (quote file)))
                                    (pair? (hash-ref j (quote tasks))))
                         (error (quote smoke) "unexpected tree JSON"))'

            # The write path validates in a fresh namespace, so it has to work
            # from the packaged binary too.
            cp ${./examples/Example.rkt} edit.rkt
            chmod u+w edit.rkt
            selfflowy add --json --no-commit --file edit.rkt "Smoke capture" > add.json
            racket -e '(require json)
                       (unless (hash-ref (call-with-input-file "add.json" read-json)
                                         (quote ok))
                         (error (quote smoke) "add failed"))'
            selfflowy check edit.rkt

            # The server has to work from the packaged binary too: static files
            # and the language readers resolve differently there.
            cp ${./examples/Example.rkt} live.rkt
            chmod u+w live.rkt

            # `serve` refuses to start without an ACP agent. The scripted one
            # from the test suite is agent enough here: real subprocess, real
            # ndjson, no LLM.
            printf '#!/bin/sh\nexec racket %s "$@"\n' \
              ${./selfflowy/tests/fake-acp-agent.rkt} > fake-acp-agent
            chmod +x fake-acp-agent
            export SELFFLOWY_ACP_AGENT="$PWD/fake-acp-agent"

            # No agent, no server: a usage error naming the variable, and
            # nothing left listening on 8098.
            if env -u SELFFLOWY_ACP_AGENT selfflowy serve --port 8098 live.rkt \
                 > refused.out 2> refused.err; then
              echo "smoke: serve started with no SELFFLOWY_ACP_AGENT" >&2
              exit 1
            fi
            grep -q SELFFLOWY_ACP_AGENT refused.err

            # Nothing to serve, no server: the DIRECTORY form globs the top
            # level, and an empty one is refused before anything binds.
            mkdir -p empty-outlines
            if selfflowy serve --port 8097 empty-outlines \
                 > refused-dir.out 2> refused-dir.err; then
              echo "smoke: serve started on a directory with no outlines" >&2
              exit 1
            fi
            grep -q empty-outlines refused-dir.err

            # Wait for a FRAMING line in a file curl is still writing. Framing
            # only — a JSON payload goes to racket below, never to grep.
            wait_for() {
              for _ in $(seq 1 150); do
                grep -q "$1" "$2" && return 0
                sleep 0.2
              done
              echo "smoke: never saw '$1' in $2" >&2
              cat "$2" >&2
              return 1
            }

            selfflowy serve --port 8099 live.rkt &
            serve_pid=$!
            for i in $(seq 1 60); do
              curl -sf -o page.html http://127.0.0.1:8099/ && break
              sleep 1
            done
            grep -qi "<html" page.html
            curl -sf -o api.json http://127.0.0.1:8099/api/tree
            racket -e '(require json)
                       (unless (= 1 (hash-ref (call-with-input-file "api.json" read-json)
                                              (quote version)))
                         (error (quote smoke) "unexpected /api/tree JSON"))'
            curl -sf -o app.css http://127.0.0.1:8099/static/app.css
            curl -sf -o collapse.js http://127.0.0.1:8099/static/collapse.js
            grep -q "selfflowy.collapsed" collapse.js
            test "$(curl -s -o /dev/null -w '%{http_code}' http://127.0.0.1:8099/nope)" = 404

            # the sidebar Today link has to be a real route
            curl -sf -o today.html http://127.0.0.1:8099/today
            grep -qi "<html" today.html

            # One stream for the rest of the run: saves and the agent both
            # ride it. It opens with a heartbeat comment, which is also how we
            # know the subscription exists before anything is pushed.
            curl -sN --max-time 120 http://127.0.0.1:8099/events > events.txt &
            events_pid=$!
            wait_for '^:hb' events.txt

            # Reload after a save. This is the check that matters in the
            # PACKAGED binary: the store loads outlines in a fresh namespace,
            # which has no collection paths to resolve selfflowy from — it has
            # to work off attached modules.
            #
            # The push comes first on purpose: a request would reload the store
            # itself, and then the watcher would have nothing to announce.
            ! grep -q "Smoke reload marker" page.html
            printf 'Smoke reload marker\n' >> live.rkt
            wait_for '^event: outline' events.txt
            curl -sf -o page2.html http://127.0.0.1:8099/
            grep -q "Smoke reload marker" page2.html

            # The agent loop, over HTTP: the page carries the panel, a POST is
            # accepted with no body of its own, and what the panel draws comes
            # back as `chat` frames on the stream above.
            grep -q 'id="sf-chat"' page.html
            test "$(curl -s -o /dev/null -w '%{http_code}' \
                      --data-urlencode 'text=smoke hello' \
                      http://127.0.0.1:8099/chat)" = 204
            wait_for '^event: chat' events.txt

            # Frames are JSON: parse them. `data:` lines carry both event
            # names' payloads, so anything that is not an object is somebody
            # else's (the outline event's revision counter).
            racket -e '(require json racket/port racket/string)
                       (define frames
                         (for*/list ([l (in-list (with-input-from-file "events.txt" port->lines))]
                                     #:when (string-prefix? l "data: ")
                                     [j (in-value
                                         (with-handlers ([exn:fail? (lambda (_e) #f)])
                                           (read-json (open-input-string (substring l 6)))))]
                                     #:when (hash? j))
                           j))
                       (unless (for/or ([f (in-list frames)])
                                 (and (equal? (hash-ref f (quote type) #f) "user")
                                      (equal? (hash-ref f (quote text) #f) "smoke hello")))
                         (error (quote smoke) "no chat frame for the prompt on /events"))'

            # Frames are ephemeral; the page is where the turn comes back. The
            # scripted agent answers "hello world".
            for i in $(seq 1 60); do
              curl -sf -o chat.html http://127.0.0.1:8099/ \
                && grep -q "hello world" chat.html && break
              sleep 0.5
            done
            grep -q "hello world" chat.html
            grep -q "smoke hello" chat.html

            # Nothing to say is not a turn.
            test "$(curl -s -o /dev/null -w '%{http_code}' \
                      --data-urlencode 'text=   ' \
                      http://127.0.0.1:8099/chat)" = 400

            # A broken file keeps the last good page (with an error banner)
            # and fails the JSON route loudly.
            printf '  @date not-a-date\n' >> live.rkt
            curl -sf -o page3.html http://127.0.0.1:8099/
            grep -q "Smoke reload marker" page3.html
            grep -q "sf-error" page3.html
            test "$(curl -s -o /dev/null -w '%{http_code}' http://127.0.0.1:8099/api/tree)" = 500

            kill $events_pid || true
            kill $serve_pid

            touch $out
          '';
      });
    };
}
