// module UI.HTML.Main

exports.setInnerHTML = function (html) {return function() {document.body.innerHTML = html; return {}; } } 
