let f = (a, b, _) => a + b

let f2 = a => f(a, 1)

let (a, b, c) = (
  string_of_int(f(1, 2, 3)),
  {
    let f3 = f2(100)
    f3(2)
  },
  {
    let arr = Array.init(2, _ => 0)
    for i in 0 to 1 {
      let f3 = f2(i)
      arr[i] = f3(2)
    }
    arr
  },
)

let () = Js.log((a, b, c))
