"use strict"

exports.readRedoxState_ = function(Just) {
  return function(Nothing) {
    return function(window) {
      return function() {
        if (typeof window.__REDOX_STATE__)
          return Just(window.__REDOX_STATE__)
        else
          return Nothing
      }
    }
  }
}

// these path are relative to `.build/Jam.App/` (client) and `output/Jam.App`
// (server)
exports.addMusicianCss = require("../../static/add-musician.css")
exports.homeCss = require("../../static/home.css")
exports.musicianCss = require("../../static/musician.css")
