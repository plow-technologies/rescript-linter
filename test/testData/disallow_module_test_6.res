// Test for pipe expressions with no extra arguments (Issue #15)
// Rule: DisallowModule[Belt.List]

// Caught: direct call with arguments
let a = Belt.List.toArray(list{1, 2, 3})

// Caught: pipe with extra arguments
let b = list{1, 2, 3}->Belt.List.map(x => x + 1)

// BUG: pipe with no extra arguments (should be caught)
let c = list{1, 2, 3}->Belt.List.toArray

// BUG: pipe with no extra arguments (should be caught)
let d = list{1, 2, 3}->Belt.List.length
