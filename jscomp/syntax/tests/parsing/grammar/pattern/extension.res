let %patternExtension = ()

let %pattern.extension = ()

let %bs.raw("x") = ()
let (%bs.raw("x") : unit) = ()
let %bs.raw("x") as y = ()

let %patExt1 | %patExt2 = ()

switch x {
| %patternExtension => ()
| %pattern.extension => ()
| %pat.stuff(test) => ()
| %pat.stuff(test) as _x => ()
| (%pat.stuff(test) : unit) => ()
| %patExt1 | %patExt2 => ()
}

let f = (%patternExtension) => ()
let f = (%pattern.extension) => ()
let f = (%bs.raw("x")) => ()
let f = (%bs.raw("x"), %bs.raw("y")) => ()
let f = (%bs.raw("x") as _y) => ()
let f = (%bs.raw("x") : unit) => ()
let f = (%patExt1 | %patExt2) => ()

for %ext in x to y { () }
for %ext1 | %ext2 in x to y { () }
for (%ext in x to y) { () }
for (%ext as e in x to y) { () }
for %ext.pattern in x to y { () }
for %ext.pattern as e in x to y { () }
for (%ext.pattern in x to y) { () }
for (%ext.pattern as e in x to y) { () }
for %ext.pattern(payload) in x to y { () }
for %ext.pattern(payload) as e  in x to y { () }
for (%ext.pattern(payload) in x to y) { () }
for (%ext.pattern(payload) as e in x to y) { () }
for (%ext.pattern(payload) as e | %ext2 as bar in x to y) { () }
