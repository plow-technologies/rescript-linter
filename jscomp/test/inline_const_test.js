// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt.js");
var Int64 = require("../../lib/js/int64.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Inline_const = require("./inline_const.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

var H = {};

var f = "hello";

var f1 = "a";

var f2 = "中文";

var f3 = "中文";

var f4 = "中文";

eq("File \"inline_const_test.res\", line 13, characters 5-12", f, "hello");

eq("File \"inline_const_test.res\", line 14, characters 5-12", f1, "a");

eq("File \"inline_const_test.res\", line 15, characters 5-12", f2, "中文");

eq("File \"inline_const_test.res\", line 16, characters 5-12", f3, "中文");

eq("File \"inline_const_test.res\", line 17, characters 5-12", f4, "中文");

eq("File \"inline_const_test.res\", line 18, characters 5-12", true, true);

eq("File \"inline_const_test.res\", line 19, characters 5-12", 1, 1);

eq("File \"inline_const_test.res\", line 20, characters 5-12", 3e-6, 0.000003);

var h = Caml_int64.add(Caml_int64.add([
          0,
          100
        ], Int64.one), Caml_int64.one);

Mt.from_pair_suites("File \"inline_const_test.res\", line 28, characters 29-36", suites.contents);

var f5 = true;

var f6 = 1;

var f7 = 3e-6;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.H = H;
exports.f = f;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.h = h;
/*  Not a pure module */