// module Utils

exports.injectBody = function (html) {return function() {document.body.innerHTML = html; return {}; } }
exports.toString = function (a) { console.log(a); return a.toString(); }

exports.platformDetect = function () {
  if (typeof window === 'object') {
    return "browser"
  } else if (typeof process === 'object'){
    return "nodejs"
  } else {
    return "unknown"
  }
}

exports.getParameterByName = function (name) {
    return function() {
        name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec(location.search);
        return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
    }
}

exports.setTitle = function(a) { return function() { document.title = a; return {}; } }

exports.parseGistResponse = function(gist) {
  var jsn = JSON.parse(gist);
  console.log(jsn);
  var filenames = Object.keys(jsn.files);
  return jsn.files[filenames[0]].content;
}
