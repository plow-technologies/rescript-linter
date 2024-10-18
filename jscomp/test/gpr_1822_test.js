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

var myShape = {
  TAG: "Circle",
  _0: 10
};

var area;

area = myShape.TAG === "Circle" ? 100 * 3.14 : Math.imul(10, myShape._1);

eq("File \"gpr_1822_test.res\", line 20, characters 3-10", area, 314);

Mt.from_pair_suites("Gpr_1822_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.myShape = myShape;
exports.area = area;
/* area Not a pure module */