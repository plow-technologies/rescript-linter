type t = (. ~x: int, ~y: string) => int

type u = (. ~x: int, ~y: string) => int

let f = (x: t): u => x

let u: u = (. ~x, ~y) => x + int_of_string(y)

let u1 = (f: u) => {
  f(. ~y="x", ~x=2)->Js.log
  f(. ~x=2, ~y="x")->Js.log
}
let h = (. ~x : unit) => 3

let a = u1(u)

type u0 = (. ~x: int=?, ~y: string) => int
