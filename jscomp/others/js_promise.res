/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/***
Deprecation note: These bindings are pretty outdated and cannot be used properly
with the `->` operator.

More details on proper Promise usage can be found here:
https://rescript-lang.org/docs/manual/latest/promise#promise-legacy
*/

@@warning("-103")

type t<+'a> = promise<'a>
type error = Js_promise2.error

/*
## Examples

```rescript
type error
```
*/

@new
external make: ((@uncurry ~resolve: (. 'a) => unit, ~reject: (. exn) => unit) => unit) => promise<
  'a,
> = "Promise"

/* `make (fun resolve reject -> .. )` */
@val @scope("Promise") external resolve: 'a => promise<'a> = "resolve"
@val @scope("Promise") external reject: exn => promise<'a> = "reject"

@val @scope("Promise") external all: array<promise<'a>> => promise<array<'a>> = "all"

@val @scope("Promise") external all2: ((promise<'a0>, promise<'a1>)) => promise<('a0, 'a1)> = "all"

@val @scope("Promise")
external all3: ((promise<'a0>, promise<'a1>, promise<'a2>)) => promise<('a0, 'a1, 'a2)> = "all"

@val @scope("Promise")
external all4: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
)> = "all"

@val @scope("Promise")
external all5: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
  'a4,
)> = "all"

@val @scope("Promise")
external all6: (
  (promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>, promise<'a5>)
) => promise<('a0, 'a1, 'a2, 'a3, 'a4, 'a5)> = "all"

@val @scope("Promise") external race: array<promise<'a>> => promise<'a> = "race"

@bs.send.pipe(: promise<'a>) external then_: (@uncurry ('a => promise<'b>)) => promise<'b> = "then"

@bs.send.pipe(: promise<'a>)
external catch: (@uncurry (error => promise<'a>)) => promise<'a> = "catch"
/* ` p|> catch handler`
    Note in JS the returned promise type is actually runtime dependent,
    if promise is rejected, it will pick the `handler` otherwise the original promise,
    to make it strict we enforce reject handler
    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch
*/

external unsafe_async: 'a => promise<'a> = "%identity"
external unsafe_await: promise<'a> => 'a = "?await"

/*
let errorAsExn (x :  error) (e  : (exn ->'a option))=
  if Caml_exceptions.isCamlExceptionOrOpenVariant (Obj.magic x ) then
     e (Obj.magic x)
  else None
[%bs.error?  ]
*/
