// The chat panel: chat frames in, DOM out. One SSE connection for the whole
// page (the body's sse-connect), so this hooks the htmx sse extension rather
// than opening an EventSource of its own — browsers cap those per origin.
//
// Frames are JSON, one per event, and land as TEXT: user text, agent chunks
// and tool titles are set with textContent, never innerHTML. The one
// exception is the `html` a `done` frame carries — Markdown the server
// rendered and sanitized (web/markdown), which replaces the plain text the
// chunks accumulated.
(function(){
  var KEY='selfflowy.chat';
  var panel,body,form,input,sink,turn,agentEl;

  // ---- open / closed (same shape as collapse.js: a class, remembered) ----
  function setOpen(o){
    panel.classList.toggle('is-open',o);
    try{localStorage.setItem(KEY,o?'1':'0')}catch(e){}
  }

  function setBusy(b){
    panel.classList.toggle('is-busy',b);
    input.disabled=b;
  }

  // ---- the message body --------------------------------------------------

  function nearBottom(){
    return body.scrollHeight-body.scrollTop-body.clientHeight<48;
  }

  // Scroll only when the reader was already at the bottom: a live turn must
  // not yank the view away from someone reading further up.
  function append(el){
    var stick=nearBottom();
    (turn||body).appendChild(el);
    if(stick)body.scrollTop=body.scrollHeight;
  }

  function line(cls,text){
    var d=document.createElement('div');
    d.className=cls;
    if(text!==undefined)d.textContent=text;
    return d;
  }

  // A turn owns its user bubble, the agent's text, and its tool lines, so a
  // tool id is only ever looked up inside the turn it belongs to.
  function startTurn(text){
    turn=null;
    var t=line('sf-chat-turn');
    t.appendChild(line('sf-chat-msg is-user',text));
    agentEl=line('sf-chat-msg is-agent');
    t.appendChild(agentEl);
    append(t);
    turn=t;
    setBusy(true);
  }

  var GLYPH={completed:'✓',failed:'✗'};

  function toolLine(id,title,status){
    var sel='[data-tool-id="'+(window.CSS&&CSS.escape?CSS.escape(id):id)+'"]';
    var el=turn?turn.querySelector(sel):null;
    if(!el){
      el=line('sf-chat-tool');
      el.setAttribute('data-tool-id',id);
      el.appendChild(line('sf-chat-tool-glyph'));
      el.appendChild(line('sf-chat-tool-title'));
      append(el);
    }
    el.setAttribute('data-status',status);
    el.querySelector('.sf-chat-tool-glyph').textContent=GLYPH[status]||'⚙';
    el.querySelector('.sf-chat-tool-title').textContent=title;
  }

  function frame(f){
    if(f.type==='user'){startTurn(f.text)}
    else if(f.type==='chunk'){
      if(!agentEl)startTurn('');
      var stick=nearBottom();
      agentEl.textContent+=f.text;
      if(stick)body.scrollTop=body.scrollHeight;
    }
    else if(f.type==='tool'){
      if(!turn)startTurn('');
      toolLine(f.id,f.title,f.status);
    }
    else if(f.type==='done'){
      if(agentEl&&typeof f.html==='string')agentEl.innerHTML=f.html;
      if(f.stopReason&&f.stopReason!=='end_turn')
        append(line('sf-chat-note',f.stopReason));
      endTurn();
    }
    else if(f.type==='error'){
      append(line('sf-chat-msg is-error',f.message));
      endTurn();
    }
    else if(f.type==='reset'){
      endTurn();
      body.textContent='';
    }
  }

  function endTurn(){turn=null;agentEl=null;setBusy(false)}

  // ---- posting -----------------------------------------------------------

  // The reply is a status, not content: what the panel draws comes back over
  // SSE, which is what keeps a second tab in step. A refusal (409 busy, 503
  // no agent) is the one thing worth saying here, and it says it inline.
  function post(url,text){
    var opts={method:'POST'};
    if(text!==undefined){
      opts.headers={'Content-Type':'application/x-www-form-urlencoded'};
      opts.body='text='+encodeURIComponent(text);
    }
    fetch(url,opts).then(function(r){
      if(r.ok)return;
      return r.text().then(function(t){
        append(line('sf-chat-msg is-error',(t||'').trim()||('http '+r.status)));
      });
    }).catch(function(e){
      append(line('sf-chat-msg is-error',String(e)));
    });
  }

  // ---- wiring ------------------------------------------------------------

  function init(){
    panel=document.getElementById('sf-chat');
    if(!panel)return;
    body=document.getElementById('sf-chat-body');
    form=document.getElementById('sf-chat-form');
    input=form.querySelector('.sf-chat-input');
    sink=document.getElementById('sf-chat-sink');
    var open='0';
    try{open=localStorage.getItem(KEY)||'0'}catch(e){}
    setOpen(open==='1');
    // A turn was still running when the page was rendered: the panel comes
    // up in the state the server says it is in, not idle. Adopt the
    // replayed turn too, or the next live frame would start a duplicate.
    if(panel.classList.contains('is-busy')){
      setBusy(true);
      var turns=body?body.querySelectorAll('.sf-chat-turn'):[];
      turn=turns[turns.length-1]||null;
      agentEl=turn?turn.querySelector('.sf-chat-msg.is-agent'):null;
    }
    if(body)body.scrollTop=body.scrollHeight;

    document.addEventListener('click',function(e){
      var t=e.target.closest('[data-post],#sf-chat-toggle');
      if(!t)return;
      e.preventDefault();
      if(t.id==='sf-chat-toggle'){setOpen(!panel.classList.contains('is-open'));return}
      post(t.getAttribute('data-post'));
    });

    form.addEventListener('submit',function(e){
      e.preventDefault();
      var text=input.value.trim();
      if(!text)return;
      input.value='';
      post(form.getAttribute('action'),text);
    });

    // The htmx sse extension would swap the frame's JSON into #sf-chat-sink.
    // Cancelling that message is how this panel borrows the page's one
    // connection: the data is ours, the swap never happens.
    document.body.addEventListener('htmx:sseBeforeMessage',function(e){
      var d=e.detail;
      if(!d||d.type!=='chat'||(sink&&d.elt!==sink))return;
      e.preventDefault();
      var f=null;
      try{f=JSON.parse(d.data)}catch(err){return}
      if(f&&f.type)frame(f);
    });
  }

  if(document.readyState==='loading')
    document.addEventListener('DOMContentLoaded',init);
  else init();
})();
