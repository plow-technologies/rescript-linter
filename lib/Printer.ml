open Rescript_parser

let formatMessage ppf src ({loc; msg} : Location.error) =
  Res_diagnostics_printing_utils.Super_location.setup_colors () ;
  (* open a vertical box. Everything in our message is indented 2 spaces *)
  (* Format.fprintf ppf "@[<v>@, %a@, %s@,@]" (print ~message_kind:`error "We've found a bug for you!") src
     loc msg; *)
  Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]"
    (Res_diagnostics_printing_utils.Super_location.print ~message_kind:`error "Lint error!" src)
    loc msg

let printError src msg d =
  formatMessage Format.err_formatter src
    Location.
      {loc= {loc_start= d.loc_start; loc_end= d.loc_end; loc_ghost= false}; msg; sub= []; if_highlight= ""}
