include (
  {
    @val external to_str: 'a => string = "JSON.stringify"
    let debug = x => print_endline(to_str(x))

    let () = {
      \"@@"(debug, 2)
      debug(1)
    }
  }: {}
)
