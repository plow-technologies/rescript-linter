let f = () => {
  let rec f = (acc, n) =>
    if n > 0 {
      f(acc + n, n - 1)
    } else {
      acc
    }
  let len = 10
  let v = Array.make(len, 0)
  for i in 0 to len - 1 {
    v[i] = f(0, i)
  }
  v
}

let suites = {
  open Mt
  list{
    ("acc", _ => Eq(f(), [0, 1, 3, 6, 10, 15, 21, 28, 36, 45])),
    ("array_to_list", _ => Eq(list{1, 2, 3}, Array.to_list([1, 2, 3]))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
