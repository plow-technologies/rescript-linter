let suites: ref<Mt.pair_suites> = ref(list{})

let test_id = ref(0)

let eq = (loc, x, y): unit => Mt.eq_suites(~test_id, loc, ~suites, x, y)

external int_size: unit => int = "%int_size"

external max_wosize: unit => int = "%max_wosize"

let v = int_size() /* 32 on JS */

eq(__LOC__, v, 32)
type backend_type =
  | Native
  | Bytecode
  | Other(string)

external get_backend_type: unit => backend_type = "%backend_type"
let backend_type = get_backend_type()

let max_array_length = max_wosize()
eq(__LOC__, backend_type, Other("BS"))

let f = () => {
  exception A(int)
  try for i in 0 to 200 {
    if i == 10 {
      raise(A(0))
    }
  } catch {
  | A(_) => ()
  }
}

Mt.from_pair_suites(__MODULE__, suites.contents)
