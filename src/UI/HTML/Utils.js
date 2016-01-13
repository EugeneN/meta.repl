// module UI.HTML.Utils


exports.appendToBody = function (node) {return function() {document.body.appendChild(node); return {}; } }
// exports.vNode2vTree = function(vnode) { return vnode }

exports.setLocationUrl = function(url) { return function() { document.location.url = url } }

exports.getBaseUrl = function() {
  return document.location.href.replace(/#.*$/, "");
}

exports.resetDisqusUnsafe = function(id) {
  return function(url) {
    return function(title) {
      return function() {
        console.log("disqus reset", id, url, title );

        setTimeout(function() {
          try {
            DISQUS.reset({
              reload: true,
              config: function () {
                this.page.identifier = id;
                this.page.url = url;
                //this.page.title = id;
                //this.language = "en";
              }
            });
          } catch (e) {
            console.log(e.toString());
          }
        }, 20); // vdom delay?
      }
    }
  }
}
