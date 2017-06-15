"use strict";

// var crypto = window.crypto && window.crypto.getRandomValues
//       || require('crypto');
var crypto = window.crypto;

exports.randomImpl = function(){
  var array = new Int32Array(4);
  crypto.getRandomValues(array);
  return array;
};
