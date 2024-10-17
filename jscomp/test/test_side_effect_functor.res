include (
  {
    module M = () => {
      let v = ref(0)
      {
        incr(v)
        print_endline(string_of_int(v.contents))
      }
      let u = 3
      let use_v = () => v.contents
      let unuse_v = () => u + 32
    }

    module N = M()

    /* let v  = N.use_v */
    let h = N.unuse_v
  }: {
    let h: unit => int
  }
)
