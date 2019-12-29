// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");

function isWithinRange(minRange, maxRange, n) {
  if (Caml_obj.caml_greaterequal(n, minRange)) {
    return Caml_obj.caml_lessequal(n, maxRange);
  } else {
    return false;
  }
}

function isSixDigitNumber(param) {
  return isWithinRange(100000, 1000000, param);
}

var wordRange = $$Array.to_list($$Array.init(6, (function (a) {
            return a;
          })));

function twoAdjacentDigitsAreTheSame(str) {
  var foundGood = false;
  for(var i = 1; i <= 5; ++i){
    foundGood = foundGood || Caml_string.get(str, i) === Caml_string.get(str, i - 1 | 0) && (i <= 1 || Caml_string.get(str, i - 2 | 0) !== Caml_string.get(str, i - 1 | 0)) && (i === 5 || Caml_string.get(str, i) !== Caml_string.get(str, i + 1 | 0));
  }
  return foundGood;
}

function digitsNeverDecrease(str) {
  var highestDigit = {
    contents: 0
  };
  var match = List.find_opt((function (a) {
          var charVal = Caml_string.get(str, a);
          var isDecreasing = charVal < highestDigit.contents;
          highestDigit.contents = charVal;
          return isDecreasing;
        }), wordRange);
  return match === undefined;
}

function program(minRange, maxRange) {
  return List.length(List.filter((function (a) {
                      if (isWithinRange(100000, 1000000, a) && twoAdjacentDigitsAreTheSame(String(a))) {
                        return digitsNeverDecrease(String(a));
                      } else {
                        return false;
                      }
                    }))($$Array.to_list($$Array.init(maxRange - minRange | 0, (function (a) {
                            return a + minRange | 0;
                          })))));
}

console.log(program(264793, 803935));

exports.isWithinRange = isWithinRange;
exports.isSixDigitNumber = isSixDigitNumber;
exports.wordRange = wordRange;
exports.twoAdjacentDigitsAreTheSame = twoAdjacentDigitsAreTheSame;
exports.digitsNeverDecrease = digitsNeverDecrease;
exports.program = program;
/* wordRange Not a pure module */