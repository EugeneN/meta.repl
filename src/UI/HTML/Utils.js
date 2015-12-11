// module UI.HTML.Utils

exports.injectBody = function (html) {return function() {document.body.innerHTML = html; return {}; } }
exports.appendToBody = function (node) {return function() {document.body.appendChild(node); return {}; } }
exports.toString = function (a) { console.log(a); return a.toString(); }
exports.vNode2vTree = function(vnode) { return vnode }
