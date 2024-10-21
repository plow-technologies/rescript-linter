// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function bool_equal(x, y) {
  if (x) {
    if (y) {
      return true;
    } else {
      return false;
    }
  } else if (y) {
    return false;
  } else {
    return true;
  }
}

function assertions(param) {
  if (!bool_equal(true, true)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "test_bool_equal.res",
            16,
            2
          ],
          Error: new Error()
        };
  }
  if (!bool_equal(false, false)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "test_bool_equal.res",
            17,
            2
          ],
          Error: new Error()
        };
  }
  if (bool_equal(true, false)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "test_bool_equal.res",
            18,
            2
          ],
          Error: new Error()
        };
  }
  if (bool_equal(false, true)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "test_bool_equal.res",
            19,
            2
          ],
          Error: new Error()
        };
  }
  
}

function f0(x) {
  if (x === true) {
    return 1;
  } else {
    return 2;
  }
}

function f1(x) {
  if (x !== true) {
    return 1;
  } else {
    return 2;
  }
}

function f2(x) {
  if (x === true) {
    return 1;
  } else {
    return 2;
  }
}

function f3(x) {
  if (x === false) {
    return 1;
  } else {
    return 2;
  }
}

function f4(x) {
  if (x !== true) {
    return 1;
  } else {
    return 2;
  }
}

function f5(x) {
  if (x) {
    return 2;
  } else {
    return 1;
  }
}

function f6(x) {
  if (x === /* [] */0) {
    return 1;
  } else {
    return 2;
  }
}

function f7(x) {
  if (x.length !== 0) {
    return 1;
  } else {
    return 2;
  }
}

function f8(x) {
  return 1;
}

exports.bool_equal = bool_equal;
exports.assertions = assertions;
exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
/* No side effect */
