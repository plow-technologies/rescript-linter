// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: "Eq",
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
}

console.log("你好");

console.log("你好");

console.log("你好");

function f(x) {
  if (x !== 333) {
    if (x >= 256) {
      return 3;
    } else {
      return 0;
    }
  } else {
    return 2;
  }
}

eq("File \"string_unicode_test.res\", line 23, characters 5-12", f(/* '{' */123), 0);

eq("File \"string_unicode_test.res\", line 24, characters 5-12", f(/* 'ō' */333), 2);

eq("File \"string_unicode_test.res\", line 25, characters 5-12", f(/* 'Ƽ' */444), 3);

Mt.from_pair_suites("string_unicode_test.res", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
/*  Not a pure module */