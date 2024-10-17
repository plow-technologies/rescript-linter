@@bs.config({
  flags: ["-bs-jsx", "4"],
})

module C0 = {
  @react.component
  let make = (~a=2, ~b=a * 2) => {
    React.int(a + b)
  }
}

module C1 = {
  @react.component
  let make = (~foo as bar="") => {
    React.string(bar)
  }
}

module C2 = {
  @react.component
  let make = (~foo as bar="", ~a=bar, ~b) => {
    React.string(bar ++ a ++ b)
  }
}

module C3 = {
  @react.component
  let make = (~priority as _, ~text="Test") => React.string(text)
}

module C4 = {
  @react.component
  let make = (~a as b, ~x=true) => b
}

module C6 = {
  module type Comp = {
    let xx : int
    @react.component
    let make: unit => React.element
  }

  @react.component
  let make = (~comp as module(Comp: Comp), ~x as (a, b)) => Comp.xx
}
