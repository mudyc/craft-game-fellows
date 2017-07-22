

(function(){
  
  // utils
  
  function query(selector) { return document.querySelector(selector); }
  function queryAll(selector) { return document.querySelectorAll(selector); }
  
  function post(url, data, success, failure){
    var request = new XMLHttpRequest();
    request.open('POST', url, true);
    request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded; charset=UTF-8');
    request.onload = function() {
          if (request.status >= 200 && request.status < 400) {
            // Success!
            //var data = JSON.parse(request.responseText);
            success(request);
          } else {
            failure(request);
          }
        };
    request.onerror = function() {
      failure(request);
    };
    request.send(data);
  }
  
  function get(url, success, failure) {
    var request = new XMLHttpRequest();
    request.open('GET', url, true);

    request.onload = function() {
      if (request.status >= 200 && request.status < 400) {
        // Success!
        //var data = JSON.parse(request.responseText);
        success(request);
      } else {
        failure(request);
      }
    };

    request.onerror = function() {
      failure(request);
    };

    request.send();
  }

  function serialize(form) {
    var data = new FormData(form);
    var ser = '';
    for (var p of data) {
      if (ser !== '')
        ser += '&';
      ser += p[0] + '=' + p[1];
    }
    return ser;
  }
  
  function each(list, cb) {
    for (var i=0, len=list.length; i<len; i++) {
      cb(list[i]);
    }
  }

  (function(){

    // language

    var lang = location.pathname.split('/')[1];
    each(queryAll('[lang]:not([lang="'+lang+'"]'), function(lang_tag){
      lang_tag.hidden = true;
    });
    query('#lang-selector option[value="'+lang+'"]').selected = true;
  })();

  (function(){

    // authentication

    get('/api/is_authenticated', 
      function(req) { // success
        var tags = document.querySelectorAll('.logged-in'), len = tags.length;
        while (len-- > 0) {
          tags[len].hidden = false;
        }
        document.querySelector('#username').textContent = req.responseText;
      },
      function(req){ // failure
        var tags = document.querySelectorAll('.logged-out'), len = tags.length;
        while (len-- > 0) {
          tags[len].hidden = false;
        }
      });

    // logout 

    document.querySelector('a[href="/api/logout"]').addEventListener('click', function(e){
      e.preventDefault();
      get('/api/logout', function(){}, function(){});
      document.cookie = 'session_id=; path=/; expires=Thu, 01 Jan 1970 00:00:01 GMT;';
      location.reload();
    });
  
  })();

  (function(){
  
    // modal
  
    var modal = document.querySelector('.modal');
    var closeButtons = document.querySelectorAll('.close-modal');
    // set open modal behaviour
    var modal_triggers = document.querySelectorAll('.open-modal');
    for (var i=0, len=modal_triggers.length; i<len; i++) {
      modal_triggers[i].addEventListener('click', function(e) {
        e.preventDefault();
        console.log(e, this, this.attribute);
        var modal = document.querySelector('#'+this.getAttribute('href'));
        modal.classList.toggle('modal-open');
      });
    }
    // set close modal behaviour
    for (i = 0; i < closeButtons.length; ++i) {
      closeButtons[i].addEventListener('click', function() {
        modal.classList.toggle('modal-open');
    	});
    }
    // close modal if clicked outside content area
    document.querySelector('.modal-inner').addEventListener('click', function() {
      modal.classList.toggle('modal-open');
    });
    // prevent modal inner from closing parent when clicked
    document.querySelector('.modal-content').addEventListener('click', function(e) {
    	e.stopPropagation();
    });
  
  })();

  (function(){
    
    // register and login
  
    query('[action="/api/register"]').addEventListener("submit", modal_submit);
    query('[action="/api/login"]').addEventListener("submit", modal_submit);
    function modal_submit(e){
      e.preventDefault();
      var form = this;
      post(form.getAttribute('action'), serialize(form),
           function(req){ // success

             // validation error
             if (req.status == 200) {
               var errors = JSON.parse(req.responseText);
               var error_tags = document.querySelectorAll('.error');
               for (var i=0, len=error_tags.length; i<len; i++) {
                 error_tags[i].hidden = true;
                 if (errors.indexOf(error_tags[i].id) != -1)
                   error_tags[i].hidden = false;
               }
             }
           
             // succesfull registration / login
             if (req.status == 204) {
               location.reload();          
             }
           }, 
           function(req){ // failure
             //alert('There was some error, sorry.');
           }
      );
    };
  
  })();
  
  (function(){

    // tutorial examples
  
    each(queryAll('.embedded-project'), function(e){
      var proj = e.dataset.project;
      
      // get sources
      (function(e, proj){
        get('/project/' + proj + '/sources.json', function(req){
          var sources = JSON.parse(req.responseText);
          (function(sources, src){
            each(sources, function(source){
                get('/project/' + proj + '/' + source, function(req){
                  src += '<div class="header">' + source + '</div>';
                  src += '<div class="source"><pre>' + req.responseText + '</pre></div>';
              
                  // get iframe for canvas
                  e.innerHTML = '<div class="example u-cf"> <iframe src="/project/' + proj + '"></iframe><div class="sources">'+src+'</div></div>';
                }, function(){});
            });
          })(sources, '');
        }, function(req){});
      })(e, proj);
    });
  })();

  (function(){

    // project html

    if (query('.project .left') != null) {
      var proj = document.body.dataset.project;
    
      function addSourceTab(src) {
        console.log('add src tab');
        var tab = document.createElement('div');
        tab.textContent = src;
        tab.classList.add('tab');
        query('.source-tabs').appendChild(tab);
      }
      function showActiveSource() {
        var js = query('.tab.active').textContent;
        get('/project/' + proj + '/'+js, function(req){
          var src = document.createElement('textarea');
          src.textContent = req.responseText;
          query('.source-file').appendChild(src);
          resize();
          CodeMirror.fromTextArea(src);
        }, function(req){});
      }
      function resize() {
        var w = window,
            d = document,
            e = d.documentElement,
            g = d.getElementsByTagName('body')[0],
            w = w.innerWidth || e.clientWidth || g.clientWidth,
            h = w.innerHeight|| e.clientHeight|| g.clientHeight;
    
        query('.right').style.height = (h - 50)+'px';
        query('.source-file').style.height = (h - 50)+'px';
      }

      get('/project/' + proj + '/sources.json', function(req){
        each(JSON.parse(req.responseText), function(source){
          addSourceTab(source);
        })
        query('.tab:first-child').classList.add('active');
        
        showActiveSource();
      }, function(){});
      
      window.addEventListener('message', function(msg){
        var line = document.createElement('div');
        line.textContent = msg.data;
        query('.console').appendChild(line);
      })
      window.addEventListener('resize', function(e){
        resize();
      })
    }
  })();
  
})()
