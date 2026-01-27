module JSON = struct
  let yojson_of_position (pos : Lexing.position) : Yojson.Safe.t =
    `Assoc
      [ ("pos_fname", `String pos.pos_fname)
      ; ("pos_lnum", `Int pos.pos_lnum)
      ; ("pos_bol", `Int pos.pos_bol)
      ; ("pos_cnum", `Int pos.pos_cnum) ]

  let position_of_yojson (json : Yojson.Safe.t) : (Lexing.position, string) result =
    match json with
    | `Assoc props -> (
        let find_string key =
          match List.assoc_opt key props with
          | Some (`String v) -> Ok v
          | _ -> Error ("Expected string for key: " ^ key)
        in
        let find_int key =
          match List.assoc_opt key props with
          | Some (`Int v) -> Ok v
          | _ -> Error ("Expected integer for key: " ^ key)
        in
        match (find_string "pos_fname", find_int "pos_lnum", find_int "pos_bol", find_int "pos_cnum") with
        | Ok fname, Ok lnum, Ok bol, Ok cnum ->
            Ok {pos_fname= fname; pos_lnum= lnum; pos_bol= bol; pos_cnum= cnum}
        | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e -> Error e )
    | _ -> Error "Expected JSON object for Position"

  let yojson_of_location (loc : Location.t) : Yojson.Safe.t =
    `Assoc
      [ ("loc_start", yojson_of_position loc.loc_start)
      ; ("loc_end", yojson_of_position loc.loc_end)
      ; ("loc_ghost", `Bool loc.loc_ghost) ]

  let location_of_yojson (json : Yojson.Safe.t) : (Location.t, string) result =
    match json with
    | `Assoc props -> (
        let find_assoc key =
          match List.assoc_opt key props with
          | Some (`Assoc v) -> Ok v
          | _ -> Error ("Expected associative array for key: " ^ key)
        in
        let find_bool key =
          match List.assoc_opt key props with
          | Some (`Bool v) -> Ok v
          | _ -> Error ("Expected boolean for key: " ^ key)
        in
        match (find_assoc "loc_start", find_assoc "loc_end", find_bool "loc_ghost") with
        | Ok start_assoc, Ok end_assoc, Ok ghost -> (
          match (position_of_yojson (`Assoc start_assoc), position_of_yojson (`Assoc end_assoc)) with
          | Ok loc_start, Ok loc_end -> Ok {Location.loc_start; loc_end; loc_ghost= ghost}
          | Error e, _ | _, Error e -> Error e )
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e )
    | _ -> Error "Expected JSON object for Location"
end

module PrettyPrint = struct
  type output = string [@@deriving yojson]

  type message_kind = [`error | `warning]

  let header_of_message_kind = function `error -> "-- Lint Error! --" | `warning -> "-- Lint Warning --"

  let print message_kind _src msg d =
    Misc.Color.setup !Clflags.color ;
    Code_frame.setup !Clflags.color ;
    Format.fprintf Format.std_formatter "@[<v>@,  %s@,  %a@,  %s@,@]"
      (header_of_message_kind message_kind)
      Location.print_loc
      Location.{loc_start= d.loc_start; loc_end= d.loc_end; loc_ghost= false}
      msg

  let asOutput message_kind _src msg d =
    let buf = Buffer.create 128 in
    let fmt = Format.formatter_of_buffer buf in
    Misc.Color.setup !Clflags.color ;
    Code_frame.setup !Clflags.color ;
    Format.fprintf fmt "@[<v>@,  %s@,  %a@,  %s@,@]"
      (header_of_message_kind message_kind)
      Location.print_loc
      Location.{loc_start= d.loc_start; loc_end= d.loc_end; loc_ghost= false}
      msg ;
    Format.pp_print_flush fmt () ;
    Buffer.contents buf

  let error src msg d = print `error src msg d

  let warning src msg d = print `warning src msg d
end

(* Location.default_error_reporter ?src:(Some (Some src)) Format.std_formatter
    Location.
      {loc= ; msg; sub= []; if_highlight= ""} *)

let printHelp () =
  print_newline () ;
  print_newline () ;
  print_endline
    "  To disable linting for certain rules, please read \
     https://github.com/plow-technologies/rescript-linter#disabling-lint"
