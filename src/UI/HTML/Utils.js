// module UI.HTML.Utils


exports.appendToBody = function (node) {return function() {document.body.appendChild(node); return {}; } }
// exports.vNode2vTree = function(vnode) { return vnode }

exports.setLocationUrl = function(url) { return function() { document.location.url = url } }
