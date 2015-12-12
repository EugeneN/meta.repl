/* jshint node: true */
"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var webpack = require("webpack-stream");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("make", function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("prebundle", ["make"], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "dist/o.js",
    module: "Main",
    main: "Main"
  });
});

gulp.task("bundle", ["prebundle"], function () {
  return gulp.src("dist/o.js")
    .pipe(webpack({
      externals: {
        fs: 'commonjs fs',
        net: "commonjs net",
        readline: "commonjs readline"
      },
      node: {
        process: false
      },
      resolve: { modulesDirectories: ["node_modules"] },
      output: { filename: "app.js" }
    }))
    .pipe(gulp.dest("public"));
});

gulp.task("default", ["bundle"]);
