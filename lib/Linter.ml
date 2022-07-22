open Rescript_parser

let processFile path =
  let channel = open_in_bin path in
  let src = really_input_string channel (in_channel_length channel) in
  close_in channel;
  src

let filename = "./test/foo.res"

let src = processFile filename

let p =
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  Res_parser.make ~mode:Res_parser.Default src filename

let structure = Res_core.parseImplementation p
let signature = Res_core.parseSpecification p

let parse acc item =
  match item with
  | () -> acc

let getIterator _callback =
  {
    Ast_iterator.default_iterator with
    structure =
      (fun iterator structure -> Ast_iterator.default_iterator.structure iterator structure);
    expr =
      (fun iterator expr ->
        (match expr with
        | {pexp_desc = Pexp_constant (Parsetree.Pconst_integer(_, _))} -> print_endline "Found integer constant"
        | _ -> ()
        );
        Ast_iterator.default_iterator.expr iterator expr);
  }

let run = match p.diagnostics with
| [] ->
    let iterator = getIterator (fun _ -> ()) in
    let () = iterator.structure iterator structure in
    print_endline "No problem"
| diagnostics -> (* parser contains problems *)
  Res_diagnostics.printReport diagnostics src
