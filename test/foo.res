// let x = 1

// let y = Some("hello")

let z1 = Some(`hello ${x->string_of_int}`)

let z2 = Some(`hello ${x |> string_of_int}`)
//
// let string_of_int = ignore
//
// let word = j`hello world`

@react.component
let make = () =>
  <div>
    <input value="value" onChange={ignore} />
  </div>
