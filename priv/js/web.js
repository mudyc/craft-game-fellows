

(function(){
  
  
  var GALLERY_DATA;
  
  
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

  var matches = function(el, selector) {
    return (el.matches || el.matchesSelector || el.msMatchesSelector || el.mozMatchesSelector || el.webkitMatchesSelector || el.oMatchesSelector).call(el, selector);
  };
  function closest(el, selector) {
    if (el == null) return null;
    if (matches(el, selector)) return el;
    return closest(el.parentNode, selector);
  }


  // API methods
  
  function api_fork(proj) {
    post('/api/fork/' + proj, "",
      function(req){
          location.href = '/' + document.body.dataset.lang + req.responseText;
      }, function(req){}
    );
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
        query('#username').textContent = req.responseText;
      },
      function(req){ // failure
        var tags = queryAll('.logged-out'), len = tags.length;
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
        closest(this, '.modal').classList.toggle('modal-open');
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

    each(queryAll('.embedded-preview'), function(e){
      var proj = e.dataset.project;
      e.innerHTML = '<iframe src="/project/' + proj + '?small"></iframe>';
    });

    each(queryAll('.embedded-project'), function(e){
      var proj = e.dataset.project;
      
      // get sources
      (function(e, proj){
        get('/project/' + proj + '/sources.json', function(req){
          var sources = JSON.parse(req.responseText);
          (function(sources, src){
            each(sources, function(source){
                get('/project/' + proj + '/' + source, function(req){
                  var example = query('.templates .example').cloneNode(true);
                  example.querySelector('li a.button').textContent = source;
                  example.querySelector('.source pre').textContent = req.responseText;
                  example.querySelector('iframe').setAttribute('src', '/project/' + proj + '?small');
                  example.querySelector('.iframe-box .fork').dataset.project = proj;
                  e.appendChild(example);
                }, function(){});
            });
          })(sources, '');
        }, function(req){});
      })(e, proj);
    });
    
    document.addEventListener('click', function(e){
      var target = e.target || e.srcElement;
      if (target.classList.contains('fork')) {
        api_fork(target.dataset.project);
      }
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
          var cm = CodeMirror.fromTextArea(src, { lineSeparator: "\n", lineNumbers: true, });
          cm.setOption("lineSeparator", "\n");
          cm.on('change', source_changed);
        }, function(req){});
      }
      var source_changed = debounce(function(cm){
        var js = active_source();
        var orig_src = cm.getValue(); //, src = orig_src.replace(/\%/g, "%25").replace(/\+/g, "%2B");
        var fd = new FormData();
        fd.append('file', js);
        fd.append('data', encodeURIComponent(orig_src));
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
        query('.source-file').innerHTML = '';
        
        get('/project/' + proj + '/assets.json', function(req){
          var data = JSON.parse(req.responseText);
          each(data, function(a){
            
            var line = query('.templates .asset-line').cloneNode(true);
            var del = line.querySelector('button');
            var name = line.querySelector('span.name');
            var img = line.querySelector('img');
            del.dataset.name = a.name;
            name.textContent = a.name;
            img.setAttribute('src', a.url);
            query('.source-file').appendChild(line);
          });

          query('.source-file').appendChild(query('.templates .asset-add').cloneNode(true));
        }, function(req){});
        // render assets json...
      }
      function show_assets() {
        query('#modal-img-assets').classList.toggle('modal-open');
      }
      function show_asset_gallery() {
        var cat = query('#modal-img-assets select option:checked').value;
        console.log(cat);
        get('/api/assets-list/'+cat, function(req){
          GALLERY_DATA = JSON.parse(req.responseText);
          GALLERY_DATA.index = 0;
          query('#modal-img-assets .count').textContent = GALLERY_DATA.length;
          asset_gallery_change(0);
        }, function(req){});
      }
      function asset_gallery_change(jmp) {
        var d = GALLERY_DATA;
        d.index = (d.index + jmp) % d.length;
        query('#modal-img-assets .index').textContent = ""+(d.index + 1);
        var category = query('#modal-img-assets select option:checked').value;
        var i = 0;
        for (var prop in d.data) {
          if (!d.data.hasOwnProperty(prop)) continue;
          if (d.index == i++) {
            query('#modal-img-assets img').setAttribute('src', '/api/asset/'+ category +'/'+d.data[prop].url);
            query('#modal-img-assets input').value = prop;
            break;
          }
        }
      }
      query('[action="/api/asset/add"]').addEventListener("submit", function(e){
        e.preventDefault();
        var form = this;
        var fd = new FormData();
        var key = query('#modal-img-assets input').value;
        var url = query('#modal-img-assets img').getAttribute('src');
        fd.append('name', key);
        fd.append('url', url);
        post(form.getAttribute('action') + '/' + proj, serialize_formdata(fd),
             function(req){ // success
               closest(form, '.modal').classList.toggle('modal-open');
               activate_assets();
             }, function(req) {
               
             });
      });
      function del_asset(elm) {
        var line = closest(elm, '.asset-line');
        var name = line.querySelector('.name').textContent;
        var fd = new FormData();
        fd.append('name', name);
        post('/api/asset/del/' + proj, serialize_formdata(fd),
             function(req){ // success
               line.parentNode.removeChild(line);
              }, function(req) {
          
              });
      }

      query('#fork').addEventListener('click', function(e){ api_fork(proj); });
      query('#delete').addEventListener('click', function(e){
        api_delete(proj);
      });

      document.addEventListener('click', function(e){
        var target = e.target || e.srcElement;
        if (target.nodeName == 'SPAN')
          target = target.parentNode;
        
        if (target.id == 'new-file-tab')
          ;//new_file_
        else if (target.id == 'assets-tab') 
          activate_assets();
        else if (target.id == 'asset-next') 
          asset_gallery_change(1);
        else if (target.id == 'asset-prev') 
          asset_gallery_change(-1);
        else if (target.id == 'add-img-asset')
          show_assets();
        else if (target.id == 'del-img-asset')
          del_asset(target);
      });
      
      document.addEventListener('change', function(e){
        var target = e.target || e.srcElement;
        console.log(target.id);
        if (target.id == 'select-asset-category')
          show_asset_gallery();
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
        else if (data.type == 'clear')
          craft_console.clear.apply(this);
        else
          craft_console.write(data);
      })
      window.addEventListener('resize', function(e){
        resize();
      })
    }
  })();
  
})()
