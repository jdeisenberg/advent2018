// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

function calcSum(runningTotal, oneLine) {
  var result = oneLine !== "" ? (
      oneLine.charAt(0) === "+" ? runningTotal + Caml_format.caml_int_of_string(oneLine.slice(1)) | 0 : runningTotal + Caml_format.caml_int_of_string(oneLine) | 0
    ) : runningTotal;
  console.log("Running total after adding " + (oneLine + (" is " + String(runningTotal))));
  return result;
}

var result = Belt_Array.reduce(Fs.readFileSync("frequencies.txt", "utf8").split("\n"), 0, calcSum);

console.log("Total: ", result);

exports.calcSum = calcSum;
exports.result = result;
/* result Not a pure module */