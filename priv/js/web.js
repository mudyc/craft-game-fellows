

(function(){
  
  // utils

  function debounce(func, wait, immediate) {
    // https://davidwalsh.name/javascript-debounce-function
    // This code is licensed with MIT and can be found from underscore
  	var timeout;
  	return function() {
  		var context = this, args = arguments;
  		var later = function() {
  			timeout = null;
  			if (!immediate) func.apply(context, args);
  		};
  		var callNow = immediate && !timeout;
  		clearTimeout(timeout);
  		timeout = setTimeout(later, wait);
  		if (callNow) func.apply(context, args);
  	};
  };
  
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
    return serialize_formdata(data);
  }
  function serialize_formdata(data) {
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
    document.body.dataset.lang = lang;
    each(queryAll('[lang]:not([lang="'+lang+'"]'), function(lang_tag){
      lang_tag.hidden = true;
    });
    query('#lang-selector option[value="'+lang+'"]').selected = true;
    query('#lang-selector').addEventListener('change', function(e){
      console.log(query('#lang-selector option:checked'));
      location.href = location.href.replace('/'+lang+'/', '/' + query('#lang-selector option:checked').value + '/');
    });
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
                  e.innerHTML = '<div class="example u-cf"> <iframe src="/project/' + proj + '?small"></iframe><div class="sources">'+src+'</div></div>';
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
      function active_source() {
        return query('.tab-nav a.active').textContent;
      }
      function add_source_tab(src) {
        //console.log('add src tab');
        var tab = document.createElement('li');
        var a = document.createElement('a');
        a.textContent = src;
        a.classList.add('button');
        tab.appendChild(a);
        query('.source-tabs').insertBefore(tab, query('.source-tabs li:nth-child(1)'));
      }
      function show_active_source() {
        var js = active_source();
        get('/project/' + proj + '/'+js, function(req){
          var src = document.createElement('textarea');
          src.textContent = req.responseText;
          query('.source-file').appendChild(src);
          resize();
          var cm = CodeMirror.fromTextArea(src, { lineSeparator: "\n" });
          cm.setOption("lineSeparator", "\n");
          cm.on('change', source_changed);
        }, function(req){});
      }
      var source_changed = debounce(function(cm){
        var js = active_source();
        var orig_src = cm.getValue(), src = orig_src.replace(/\+/g, "%2B");
        var fd = new FormData();
        fd.append('file', js);
        fd.append('data', encodeURI(src));
        post('/api/edit/' + proj, serialize_formdata(fd),
          function(req){
            craft_console.clear();
            query('iframe').src += '';
            try {
              // While nice try we get better error reporting from iframe itself
              new Function(orig_src);
              // if ok, we can reload the iframe..

              //console.log("reload iframe");
              //query('iframe').src += '';
            } catch (e) {
              //craft_console.error(e);
              //console.log("there is some error: "+e);
            }
          }, function(req){});
      }, 1000);

      function activate_assets() {
        each(queryAll('.source-tabs li a'), function(a){ a.classList.remove('active'); });
        query('#assets-tab').classList.add('active');
        // render assets json...
      }
      
      query('#fork').addEventListener('click', function(e){
        post('/api/fork/' + proj, "",
          function(req){
              location.href = '/' + document.body.dataset.lang + req.responseText;
          }, function(req){}
        );
      });
      
      document.addEventListener('click', function(e){
        var target = e.target || e.srcElement;
        if (target.id == 'new-file-tab')
          ;//new_file_
        else if (target.id == 'assets-tab') 
          activate_assets();
      });
      
      function resize() {
        var w = window,
            d = document,
            e = d.documentElement,
            g = d.getElementsByTagName('body')[0],
            w = w.innerWidth || e.clientWidth || g.clientWidth,
            h = w.innerHeight|| e.clientHeight|| g.clientHeight;
            ;
    
        //query('.right').style.height = (h - 50)+'px';
        query('.source-file').style.height = (h - query('.source-file').offsetTop)+'px';
        query('iframe').style.height = ((h - query('iframe').offsetTop)*2/3) + 'px';
      }

      get('/project/' + proj + '/sources.json', function(req){
        each(JSON.parse(req.responseText), function(source){
          add_source_tab(source);
        })
        query('.tab-nav li:first-child a').classList.add('active');
        
        show_active_source();
      }, function(){});
      
      craft_console = {
        clear: function() {
          query('.console').innerHTML = '';
        },
        write: function(msg) {
          var line = document.createElement('div');
          line.textContent = msg;
          query('.console').appendChild(line);
        },
        error: function(err) {
          console.log(err.stack);         
          var line = document.createElement('div');
          line.classList.add('error');
          var place = document.createElement('span');
          place.textContent = '('+err.url.split(proj)[1] + ':' + err.line+')';
          line.textContent = err.msg;
          line.appendChild(place);
          query('.console').appendChild(line);          
        },
        log: function() {
          var args = Array.from(arguments );
          console.log(args, arguments);
          var line = document.createElement('div');
          line.textContent = '';
          for (var i=0; i<args.length; i++) {
            if (i>0)
              line.textContent += ', ';
            line.textContent += JSON.stringify(args[i]);
          }
          query('.console').appendChild(line);
        },
      };
      window.addEventListener('message', function(msg){
        data = JSON.parse(msg.data);
        console.log(msg, data);
        if (data.type == 'error')
          craft_console.error(data);
        else if (data.type == 'log')
          craft_console.log.apply(this, data.args);
        else
          craft_console.write(data);
      })
      window.addEventListener('resize', function(e){
        resize();
      })
    }
  })();
  
})()
