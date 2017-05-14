"use strict"

const browserify = require("browserify")
const gulp = require("gulp")
const gutil = require("gulp-util")
const nodemon = require("gulp-nodemon")
const purescript = require("gulp-purescript")
const source = require('vinyl-source-stream')
const sourcemaps = require('gulp-sourcemaps')
const watchify = require("watchify")

const b = watchify(browserify({
  entries: ['.build/Jam.App/index.js']
}))
b.plugin(require('css-modulesify'), {
  output: "./dist/static/style.css",
  after: [
    require("postcss-import"),
    require("postcss-url"),
    require("postcss-assets"),
    require("postcss-custom-media"),
    require("postcss-modules-values"),
    require("postcss-will-change"),
    require("postcss-nested"),
    require("postcss-cssnext"),
  ]
})
b.on("update", bundle)
b.on("log", gutil.log.bind(gutil, "browserify log:"))

function bundle() {
  return b.bundle()
    // log errors if they happen
    .on('error', gutil.log.bind(gutil, 'Browserify Error'))
    .pipe(source('index.js'))
    // optional, remove if you dont want sourcemaps
    // .pipe(sourcemaps.init({loadMaps: true})) // loads map from browserify file
    // Add transformation tasks to the pipeline here.
    .pipe(sourcemaps.write('./')) // writes .map file
    .pipe(gulp.dest('./dist/static/'));
}

gulp.task("compile-purescript", () => {
  return purescript.compile({
    src: ["src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"]
  })
})

gulp.task("bundle-app", ["compile-purescript"], () => {
  return purescript.bundle({
    src: "output/**/*.js",
    output: ".build/Jam.App/index.js",
    module: "Jam.App",
    main: "Jam.App",
  })
})

gulp.task("browserify", ["bundle-app"], bundle)

gulp.task("bundle-server", ["compile-purescript"], () => {
  return purescript.bundle({
    src: "output/**/*.js",
    output: ".build/Jam.Server/index.js",
    module: "Jam.Server",
    main: "Jam.Server",
  })
})

gulp.task("watch", ["browserify", "bundle-server"], () => {
  gulp.watch(["src/**/*.purs", "src/**/*.js"], ["compile-purescript"])
  gulp.watch(["output/**/*.js"], ["bundle-app", "bundle-server"])
  gulp.watch([".build/Jam.App/index.js"], ["browserify"])
  nodemon(({
    script: "./start.js",
    watch: ".build/Jam.Server",
  }))
})
