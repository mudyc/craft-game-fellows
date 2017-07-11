(function(){
  // language
  var lang = location.pathname.split('/')[1];
  var lang_tags = document.querySelectorAll('[lang]:not([lang="'+lang+'"]');
  var len = lang_tags.length;
  while (len-- > 0) {
    lang_tags[len].hidden = true;
  }
  document.querySelector('#lang-selector option[value="'+lang+'"]').selected = true;
})();

(function(){
  // authentication
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

  get('/api/is_authenticated', 
    function(req) { // success
      var tags = document.querySelectorAll('.logged-in'), len = tags.length;
      while (len-- > 0) {
        tags[len].hidden = false;
      }
    },
    function(req){ // failure
      var tags = document.querySelectorAll('.logged-out'), len = tags.length;
      while (len-- > 0) {
        tags[len].hidden = false;
      }
    });

  
})();

