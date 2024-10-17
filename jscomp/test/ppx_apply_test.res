let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let u = ((. a, b) => a + b)(. 1, 2)

let nullary = (. ()) => 3

let unary = (. a) => a + 3

let xx = unary(. 3)
let () = eq(__LOC__, u, 3)

@val external f: (. int) => int = "xx"

let h = a => f(. a)

Mt.from_pair_suites(__MODULE__, suites.contents)
