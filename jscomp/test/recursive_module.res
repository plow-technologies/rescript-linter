let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

module rec Int32: {
  type t
  type buffer
  @get external buffer: t => buffer = "buffer"
  @get_index external get: (t, int) => int = ""
  @set_index external set: (t, int, int) => unit = ""
  @new external create: array<int> => t = "Int32Array"
  @new external of_buffer: buffer => t = "Int32Array"
} = Int32 /* Int32 is compiled away in 4.06 */

module Xx: {
  let f: (int, int) => int
} = {
  external f: (int, int) => int = "?hfiehi"
}

let uuu = Xx.f

module rec Int3: {
  let u: int => int
} = Int3

module rec Inta: {
  let a: lazy_t<int>
} = {
  let a = lazy Lazy.force(Intb.a)
}
and Intb: {
  let a: lazy_t<int>
} = {
  let a = lazy (Lazy.force(Inta.a) + 1)
}

eq(
  __LOC__,
  -1,
  try Lazy.force(Intb.a) catch {
  | Lazy.Undefined => -1
  },
)

module A = {
  module rec Inta: {
    let a: lazy_t<int>
  } = {
    let a = lazy (Lazy.force(Intb.a) + 1)
  }
  and Intb: {
    let a: lazy_t<int>
  } = {
    let a = lazy 2
  }
}

eq(__LOC__, Lazy.force(A.Inta.a), 3)
/* expect raise Undefined_recursive_module */
eq(
  __LOC__,
  4,
  try {
    ignore(Int3.u(3))
    3
  } catch {
  | Undefined_recursive_module(_) => 4
  },
)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
