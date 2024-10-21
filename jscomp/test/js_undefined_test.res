open Js_undefined

let suites = {
  open Mt
  list{
    ("toOption - empty", _ => Eq(None, empty |> toOption)),
    (__LOC__, _ => Eq(None, return() |> toOption)),
    ("return", _ => Eq(Some("something"), return("something") |> toOption)),
    ("test - empty", _ => Eq(true, empty == Js.undefined)),
    (__LOC__, _ => Eq(true, return() == Js.undefined)),
    ("bind - empty", _ => Eq(empty, bind(empty, (. v) => v))),
    ("bind - 'a", _ => Eq(return(4), bind(return(2), (. n) => n * 2))),
    (
      "iter - empty",
      _ => {
        let hit = ref(false)
        let _ = iter(empty, (. _) => hit := true)
        Eq(false, hit.contents)
      },
    ),
    (
      "iter - 'a",
      _ => {
        let hit = ref(0)
        let _ = iter(return(2), (. v) => hit := v)
        Eq(2, hit.contents)
      },
    ),
    ("fromOption - None", _ => Eq(empty, None |> fromOption)),
    ("fromOption - Some", _ => Eq(return(2), Some(2) |> fromOption)),
  }
}
Mt.from_pair_suites(__MODULE__, suites)
