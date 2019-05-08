"use strict";

var crypto = window.crypto || window.msCrypto;

exports._randomState = function(){
  var ans = new Int32Array(4);
  crypto.getRandomValues(ans);
  return {value0: ans[0], value1: ans[1],
          value2: ans[2], value3: ans[3]
         };
};
