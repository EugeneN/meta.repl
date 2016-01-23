// module UI.HTML.Utils


exports.appendToBody = function (node) {return function() {document.body.appendChild(node); return {}; } }
// exports.vNode2vTree = function(vnode) { return vnode }

exports.setLocationUrl = function(url) { return function() { document.location.url = url } }

exports.getBaseUrl = function() {
  return document.location.href.replace(/#.*$/, "");
}

exports.highlightCodeUnsafe = function() {
  try {
    Prism.highlightAll();
    console.log("Prism success")
  } catch(e) {
    console.log("Prism error:", e)
  }

}

exports.resetDisqusUnsafe = function(id) {
  return function(url) {
    return function(title) {
      return function() {
        setTimeout(function() {
          console.log("disqus reset", id, url, title );
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
exports.resetLivefyreUnsafe = function(id) {
  return function(url) {
    return function(title) {
      return function() {
        setTimeout(function() {
          console.log("livefyre reset", id, url, title );
          //try {
            if(window.fyre) {
              var articleId = fyre.conv.load.makeArticleId(null);
              fyre.conv.load({}, [{
                  el: 'livefyre-comments',
                  network: "livefyre.com",
                  siteId: "380346",
                  articleId: id,
                  signed: false,
                  collectionMeta: {
                      articleId: id,
                      url: fyre.conv.load.makeCollectionUrl(),
                  }
              }], function() {});
            } else {
              console.log("fype not available");
            }
          // } catch (e) {
          //   console.log(e.toString());
          // }
        }, 20); // vdom delay?
      }
    }
  }
}
