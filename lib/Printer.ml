let printError src msg d =
  Misc.Color.setup !Clflags.color ;
  Code_frame.setup !Clflags.color ;
  Format.fprintf Format.std_formatter "@[<v>@,  %a@,  %s@,@]"
    (Location.print ~src:(Some src) ~message_kind:`error "Lint Error!")
    Location.{loc_start= d.loc_start; loc_end= d.loc_end; loc_ghost= false}
    msg ;
  List.iter
    (Format.fprintf Format.std_formatter "@,@[%a@]" (Location.default_error_reporter ~src:(Some src)))
    []

(* Location.default_error_reporter ?src:(Some (Some src)) Format.std_formatter
    Location.
      {loc= ; msg; sub= []; if_highlight= ""} *)

let printHelp () =
  print_newline () ;
  print_newline () ;
  print_endline
    "  To disable linting for certain rules, please read \
     https://github.com/plow-technologies/rescript-linter#disabling-lint"
