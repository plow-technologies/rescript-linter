let x: string = %raw(`"\\x01\\x02\\x03"`)

let max: (. float, float) => float = %raw("Math.max")

let u = v => max(. 1., v)
/* let max2 : float -> float -> float = [%bs.raw {Math.max} ] */
%%raw(`

function $$test(x,y){
  return x + y;
}
`)

let regression3: (. float, float) => float = %raw("Math.max")

let regression4: (. float, (. float) => float) => float = %raw("Math.max")
let g = a => {
  let regression: (float, string => 'a) => string = %raw(`function(x,y){
   return ""
}`)

  let regression2: (float, float) => float = %raw("Math.max")
  \"@@"(ignore, regression(a, failwith))
  \"@@"(ignore, regression2(3., 2.))
  \"@@"(ignore, regression3(. 3., 2.))
  \"@@"(ignore, regression4(.3., (. x) => x))
}

let max2: (. float, float) => float = %raw("Math.max")

let umax = (a, b) => max2(. a, b)
let u = h => max2(. 3., h)

let max3: (. (float, float)) => float = %raw("Math.max")
let uu = h => max2(. 3., h)

@val("$$test") external test: (int, int) => int = ""

let empty = (%raw(` Object.keys`): (. _) => array<string>)(. 3)

let v = test(1, 2)

/* type v = width:int -> int [@bs] */
/* class type t = object */
/* method exit : ?code:int -> unit -> unit */
/* end [@bs] */
/* see #570 */

type vv = (. int) => int

Mt.from_pair_suites(
  __MODULE__,
  {
    open Mt
    list{
      ("unsafe_max", _ => Eq(2., max(. 1., 2.))),
      ("unsafe_test", _ => Eq(3, v)),
      ("unsafe_max2", _ => Eq(2, (%raw(`Math.max`): (. int, int) => int)(. 1, 2))),
      ("ffi_keys", _ => Eq(["a"], Ffi_js_test.keys(. %raw(` {a : 3}`)))),
    }
  },
)
