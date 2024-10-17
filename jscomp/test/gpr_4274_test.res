module N = {
  type t = {x: int}
}

module type X = {
  type f<'a> = {i: 'a}
  let forEach: (. array<'a>, f<'a => unit>) => unit
}

/* type annotation here interferes.. */
let f = (module(X: X), xs: array<N.t>) => X.forEach(. xs, {X.i: x => Js.log(x.x)})

Belt.List.forEachU(list{{N.x: 3}}, (. x) => Js.log(x.x))

module Foo = {
  type record = {foo: string}
}
let bar = [{Foo.foo: @reason.raw_literal("bar") "bar"}]

let _ = Belt.Array.mapU(bar, (. b) => b.foo)
