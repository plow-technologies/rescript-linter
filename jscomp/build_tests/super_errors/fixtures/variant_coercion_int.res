type x = | @as(1) One(bool) | @as(2) Two

let x = One(true)

let y = (x :> int)
