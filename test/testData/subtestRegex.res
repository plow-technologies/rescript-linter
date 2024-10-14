open Disallowed_embedded_regex_literal

describe("Tests that regex tests are detected recursively", () => {
  test("someFile.jpg", () => {
    Js.Re.test_(isAnImageRe, "someFile.jpg")->expect->toEqual(true)
  })
})
