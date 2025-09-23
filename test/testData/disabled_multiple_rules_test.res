// RSLINT_DISABLE_DisallowFunction[string_of_int]
// RSLINT_DISABLE_DisallowFunction[intOfStringOpt]
// RSLINT_DISABLE_DisallowDeadCode

let _ = string_of_int(0)
let _ = intOfStringOpt("1")
let _ = "hello" -> React.string

@dead
let x = @dead "world"