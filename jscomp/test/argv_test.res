let anno_fun = arg => ()
/* Js.log arg */
let usage_msg = "Usage:\n"
let compile = ref(false)
let test = ref(true)
let arg_spec = {
  open Arg
  list{("-c", Set(compile), " Compile"), ("-d", Clear(test), " Test")}
}

Arg.parse_argv(["prog.exe", "-c", "-d"], arg_spec, anno_fun, usage_msg)

{
  assert (compile.contents == true)
  assert (test.contents == false)
}
