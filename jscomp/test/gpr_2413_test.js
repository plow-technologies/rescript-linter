// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function f(x) {
  switch (x.TAG) {
    case "A" :
        var a = x._0;
        if (a.TAG === "P") {
          var a$1 = a._0;
          return a$1 + a$1 | 0;
        }
        var a$2 = a._0;
        return a$2 - a$2 | 0;
    case "B" :
    case "C" :
        break;
    
  }
  var a$3 = x._0._0;
  return Math.imul(a$3, a$3);
}

function ff(c) {
  c.contents = c.contents + 1 | 0;
  var match = (1 + c.contents | 0) + 1 | 0;
  if (match > 3 || match < 0) {
    return 0;
  } else {
    return match + 1 | 0;
  }
}

exports.f = f;
exports.ff = ff;
/* No side effect */