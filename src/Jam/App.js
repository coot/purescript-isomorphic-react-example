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
