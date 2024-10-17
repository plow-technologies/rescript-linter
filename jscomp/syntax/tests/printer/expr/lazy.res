let x = lazy sideEffect

// parens
let x = lazy true
let x = lazy 12
let x = lazy (12: int)
let x = lazy 12
let x = lazy list{1, 2, ...x}
let x = lazy module(Foo: Bar)
let x = lazy module(Foo)
let x = lazy Rgb(1, 2, 3)
let x = lazy [a, b, c]
let x = lazy {x: 1, y: 3}
let x = lazy (1, 2, 3)
let x = lazy %extension
let x = lazy user.name
let x = lazy streets[0]
let x = lazy apply(arg1, arg2)
let x = lazy apply(. arg1, arg2)
let x = lazy -1
let x = lazy !true
let x = lazy (x => print(x))
let x = lazy (switch x {
  | Blue => ()
  | Yello => ()
})

let x = lazy (for i in 0 to 10 {
  print_int(i)
})

let x = lazy (if i < 10 {
  print_int(i)
} else {
  print_int(1000)
})

let x = lazy (while i < 10 {
  print_int(i)
})

let x = lazy (assert(false))
let x = lazy (try sideEffect() catch {| Exit => ()})

let x = lazy (@attr expr)

let x = lazy (a + b)

let x = @attr lazy x

let x = lazy street["number"]
let x = lazy streets[0]

lazy (address["street"] = "Brusselsestraat")

lazy (true ? 0 : 1)
