// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Functor_def = require("./functor_def.js");
var Functor_inst = require("./functor_inst.js");

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

var Y0 = Functor_def.Make(Functor_inst);

var Y1 = Functor_def.Make(Functor_inst);

eq("File \"functor_app_test.res\", line 15, characters 3-10", Curry._2(Y0.h, 1, 2), 4);

eq("File \"functor_app_test.res\", line 16, characters 3-10", Curry._2(Y1.h, 2, 3), 6);

var v = Functor_def.$$return();

eq("File \"functor_app_test.res\", line 20, characters 3-10", v, 2);

Mt.from_pair_suites("Functor_app_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Y0 = Y0;
exports.Y1 = Y1;
exports.v = v;
/* Y0 Not a pure module */