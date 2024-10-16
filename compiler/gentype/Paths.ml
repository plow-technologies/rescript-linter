open GenTypeCommon

let concat = Filename.concat

let handle_namespace cmt =
  let cut_after_dash s =
    match String.index s '-' with
    | n -> String.sub s 0 n [@doesNotRaise]
    | exception Not_found -> s
  in
  let no_dir = Filename.basename cmt = cmt in
  if no_dir then
    cmt |> (Filename.chop_extension [@doesNotRaise]) |> cut_after_dash
  else
    let dir = cmt |> Filename.dirname in
    let base =
      cmt |> Filename.basename |> (Filename.chop_extension [@doesNotRaise])
      |> cut_after_dash
    in
    Filename.concat dir base

let find_name_space cmt =
  let keep_after_dash s =
    match String.index s '-' with
    | n ->
      Some ((String.sub s (n + 1) [@doesNotRaise]) (String.length s - n - 1))
    | exception Not_found -> None
  in
  cmt |> Filename.basename |> (Filename.chop_extension [@doesNotRaise])
  |> keep_after_dash

let get_output_file_relative ~config cmt =
  (cmt |> handle_namespace) ^ ModuleExtension.ts_input_file_suffix ~config

let get_output_file ~(config : Config.t) cmt =
  Filename.concat config.project_root (get_output_file_relative ~config cmt)

let get_module_name cmt =
  cmt |> handle_namespace |> Filename.basename |> ModuleName.from_string_unsafe

let get_cmt_file cmt =
  let path_cmt = Filename.concat (Sys.getcwd ()) cmt in
  let cmt_file =
    if Filename.check_suffix path_cmt ".cmt" then
      let path_cmt_lower_case =
        let dir_name = path_cmt |> Filename.dirname in
        let base_name = path_cmt |> Filename.basename in
        Filename.concat dir_name (base_name |> String.uncapitalize_ascii)
      in
      let path_cmti =
        (Filename.chop_extension path_cmt [@doesNotRaise]) ^ ".cmti"
      in
      let path_cmti_lower_case =
        (Filename.chop_extension path_cmt_lower_case [@doesNotRaise]) ^ ".cmti"
      in
      if Sys.file_exists path_cmti_lower_case then path_cmti_lower_case
      else if Sys.file_exists path_cmti then path_cmti
      else if Sys.file_exists path_cmt_lower_case then path_cmt_lower_case
      else if Sys.file_exists path_cmt then path_cmt
      else ""
    else ""
  in
  cmt_file

let get_config_file ~project_root =
  let config = concat project_root Config.compiler_config_file in
  match config |> Sys.file_exists with
  | true -> Some config
  | false -> (
    let config = concat project_root Config.legacy_compiler_config_file in
    match config |> Sys.file_exists with
    | true -> Some config
    | false -> None)

let read_config ~namespace = Config.read_config ~get_config_file ~namespace
