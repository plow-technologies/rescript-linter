// Test that Pexp_apply scenarios (function calls) are still caught after Issue #15 fix
// Rule: DisallowModule[Belt.List] + DisallowModule[Belt.Array]

// 1. Direct call with single argument (Pexp_apply, Belt.List)
let a = Belt.List.toArray(list{1, 2, 3})

// 2. Direct call with multiple arguments (Pexp_apply, Belt.List)
let b = Belt.List.has(list{1, 2, 3}, 1, (a, b) => a == b)

// 3. Pipe with extra arguments (Pexp_apply, Belt.List)
let c = list{1, 2, 3}->Belt.List.map(x => x + 1)

// 4. Pipe with no extra arguments (Pexp_ident, Belt.List — was the bug)
let d = list{1, 2, 3}->Belt.List.toArray

// 5. Chained pipe: no-args into with-args (Belt.List missed + Belt.Array caught)
let e = list{1, 2, 3}->Belt.List.toArray->Belt.Array.map(x => x)

// 6. Nested calls (both inner and outer are Pexp_apply, Belt.List)
let f = Belt.List.toArray(Belt.List.map(list{1, 2, 3}, x => x + 1))
