// Test for disallowing Belt module (should catch Belt.List and Belt.Array)
let x = Belt.List.toArray([1, 2, 3])
let y = Belt.Array.map([1, 2, 3], x => x + 1)
let z = Belt.Option.getWithDefault(None, 0)
