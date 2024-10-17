type t = string

let curry = "Curry"
let rescriptPervasives = "RescriptPervasives"

let sanitizeId s =
  let s =
    if String.contains s '.' || String.contains s '[' || String.contains s ']'
    then
      s
      |> String.map (function
           | '.' | '[' | ']' -> '_'
           | c -> c)
    else s
  in
  if s <> "" && (s.[0] [@doesNotRaise]) >= 'A' && (s.[0] [@doesNotRaise]) <= 'z'
  then s
  else "_" ^ s

let forJsFile s = sanitizeId s ^ "JS"

let forInnerModule ~fileName ~innerModuleName =
  (fileName |> forJsFile) ^ "." ^ innerModuleName

let fromStringUnsafe s = s
let toString s = s
let compare (s1 : string) s2 = compare s1 s2
let uncapitalize = String.uncapitalize_ascii
