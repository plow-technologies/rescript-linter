let map = (f, a) => {
  let l = Array.length(a)
  if l == 0 {
    []
  } else {
    let r = Array.make(l, f(. Array.unsafe_get(a, 0)))
    for i in 1 to l - 1 {
      Array.unsafe_set(r, i, f(. Array.unsafe_get(a, i)))
    }
    r
  }
}

let map = (type u v, f: u => v, a: array<u>): array<v> => map((. x) => f(x), a)

let init = (l, f) =>
  if l == 0 {
    []
  } else if l < 0 {
    invalid_arg("Array.init")
  } else {
    /* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... */

    let res = Array.make(l, f(. 0))
    for i in 1 to pred(l) {
      Array.unsafe_set(res, i, f(. i))
    }
    res
  }

let init = (l, f) => init(l, (. x) => f(x))

let fold_left = (f, x, a) => {
  let r = ref(x)
  for i in 0 to Array.length(a) - 1 {
    r := f(. r.contents, Array.unsafe_get(a, i))
  }
  r.contents
}

let fold_left = (f, x, a) => fold_left((. x, y) => f(x, y), x, a)

let f2 = () => {
  let arr = init(3_000_000, i => float_of_int(i))
  let b = map(i => i +. i -. 1., arr)
  let v = fold_left(\"+.", 0., b)
  print_endline(string_of_float(v))
}

f2()

/* local variables: */
/* compile-command: "ocamlc bench.ml -o bench.byte  && js_of_ocaml --opt 3 bench.byte -o bench.1.js " */
/* end: */
