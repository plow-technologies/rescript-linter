let f = x => {
  let unused_f = x => {
    Js.log("x")
    x + x
  }
  let unused_h = x => unused_f(3)
  x + 3
}

module M = (S: Set.S): {
  let f: int => int
} => {
  let f = x => x
  let unused_g = x => x + 2
  let unused_h = x => unused_g(x)
}

let fff = (_, _) => 3
