let x: int = @dead 12

let add = (a, b) => (@dead a) + b

type myVariant =
  | @dead A({@dead x: int, @dead y: string})
  | B
  | @dead C

// Doesn't error on @dead inside comments
/* @dead */
// @dead let x = 10

@dead
let rec add2 = (a: int) => add(a, 2)
and @dead add3 = (@dead b: int) => add2(10)

let final = add(x, 10)

@dead
module MyModule = {
  @dead
  let a = 2
}

@dead
type myRecord = {
  @dead
  field1: int,
  @dead
  field2: string,
}

