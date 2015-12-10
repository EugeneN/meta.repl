// module UI.HTML.Main

exports.setInnerHTML = function (html) {return function() {document.body.innerHTML = html; return {}; } }
exports.toString = function (a) {
  console.log(a)
  return a.toString() }
