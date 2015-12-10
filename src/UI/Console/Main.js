// module UI.Console.Main

exports.exportGlobal = function (fname) {return function(f) {return function() {window[fname] = f; return {}; } } }
