module Regex = {
  let ip4Re = %re(
    "/(((2(5[0-5]|[0-4][0-9])|[01]?[0-9][0-9]?)\.){3}(2(5[0-5]|[0-4][0-9])|[01]?[0-9][0-9]?))$/"
  )
}

describe("IPv4 without subnet", () => {
  test("127.0.0.1", () => {
    Js.Re.test_(Regex.ip4Re, "127.0.0.1")->expect->toEqual(true)
  })
})
