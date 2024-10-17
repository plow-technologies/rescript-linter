// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function u(b) {
  if (typeof b !== "object" && b === "A") {
    return 0;
  } else {
    return 1;
  }
}

function u1(b) {
  if (typeof b !== "object" && b === "A") {
    return true;
  } else {
    return false;
  }
}

function u2(b) {
  if (typeof b !== "object" && b === "A") {
    return false;
  } else {
    return true;
  }
}

Mt.eq_suites(test_id, suites, "File \"gpr_4924_test.res\", line 27, characters 29-36", false, false);

Mt.eq_suites(test_id, suites, "File \"gpr_4924_test.res\", line 28, characters 29-36", true, true);

Mt.eq_suites(test_id, suites, "File \"gpr_4924_test.res\", line 29, characters 29-36", true, true);

function u3(b) {
  if (typeof b !== "object" && b === "A") {
    return 3;
  } else {
    return 4;
  }
}

function u4(b) {
  if (typeof b !== "object" && b === "A") {
    return 3;
  } else {
    return 4;
  }
}

function u5(b) {
  if (typeof b !== "object" && b === "A") {
    return false;
  } else {
    return true;
  }
}

function u6(b) {
  if (typeof b !== "object" && b === "A") {
    return true;
  } else {
    return false;
  }
}

Mt.from_pair_suites("File \"gpr_4924_test.res\", line 54, characters 17-24", suites.contents);

var from_pair_suites = Mt.from_pair_suites;

var eq_suites = Mt.eq_suites;

exports.from_pair_suites = from_pair_suites;
exports.eq_suites = eq_suites;
exports.suites = suites;
exports.test_id = test_id;
exports.u = u;
exports.u1 = u1;
exports.u2 = u2;
exports.u3 = u3;
exports.u4 = u4;
exports.u5 = u5;
exports.u6 = u6;
/*  Not a pure module */
