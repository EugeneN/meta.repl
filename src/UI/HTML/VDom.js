// module UI.HTML.VDom

exports.showVNodeP = JSON.stringify;

var vn = require('virtual-dom').VNode;
exports.vnodeP = function(a,b,c) { return new vn(a,b,c); }

var vt = require('virtual-dom').VText;
exports.vtext = function(a) { return new vt(a); }

var Entities = require('html-entities').AllHtmlEntities;
var entities = new Entities();

exports.ventity = function(a) {
  console.log("ventity: ", entities.decode(a))
  return new vt(entities.decode(a));
}

var rewriteAttr = function(a) { return (a === "class") ? "className" : a; }
exports.convertAttrsP = function(toList, attrs) {
  var out = {};
  toList(attrs).forEach(function(t) {
    out[rewriteAttr(t.value0)] = t.value1;
  });
  return out;
}
