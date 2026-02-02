// Test qualified module paths with Belt.Result
let x = Belt.Result.Ok(42)
let y = Belt.Result.Error("failed")

let z = switch x {
| Belt.Result.Ok(value) => value
| Belt.Result.Error(_) => 0
}

let a = Belt.Result.map(x, v => v + 1)
