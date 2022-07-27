open Rescript_parser

module Rule : Rule.HASRULE with type t = Parsetree.expression = struct
  type t = Parsetree.expression

  let proxy = Rule.MExpression

  let meta =
    { Rule.ruleName= "NoJStringInterpolation"
    ; Rule.ruleDescription=
        "[Rescript] Do not use j`<string>` interpolation, use `` instead and explicitly convert args to \
         string." }

  let lint expr =
    match expr with
    | { Parsetree.pexp_desc= Pexp_apply ({pexp_desc= Pexp_ident {txt= Longident.Lident "div"}}, _)
      ; Parsetree.pexp_attributes= [({Asttypes.txt= "JSX"}, _)]
      ; Parsetree.pexp_loc= loc } ->
        Rule.LintError (meta.ruleDescription, loc)
    | _ -> Rule.LintOk
end

(* [ structure_item (test.res[12,167+0]..[16,237+8]) Pstr_value Nonrec [ <def> attribute "react.component"
   (test.res[12,167+0]..[12,167+16]) [] pattern (test.res[13,184+4]..[13,184+8]) Ppat_var "make"
   (test.res[13,184+4]..[13,184+8]) expression (test.res[13,184+11]..[16,237+8]) Pexp_fun Nolabel None pattern
   (test.res[13,184+11]..[13,184+13]) Ppat_construct "()" (test.res[13,184+11]..[13,184+13]) None expression
   (test.res[14,201+3]..[16,237+8]) attribute "JSX" (_none_[1,0+-1]..[1,0+-1]) ghost [] Pexp_apply expression
   (test.res[14,201+3]..[14,201+6]) Pexp_ident "div" (test.res[14,201+3]..[14,201+6]) [ <arg> Labelled
   "children" expression (test.res[14,201+6]..[16,237+2]) Pexp_construct "::" (test.res[15,209+5]..[16,237+2])
   Some expression (test.res[15,209+5]..[16,237+2]) Pexp_tuple [ expression (test.res[15,209+5]..[15,209+27])
   attribute "JSX" (_none_[1,0+-1]..[1,0+-1]) ghost [] Pexp_apply expression (test.res[15,209+5]..[15,209+10])
   Pexp_ident "input" (test.res[15,209+5]..[15,209+10]) [ <arg> Labelled "value" expression
   (test.res[15,209+17]..[15,209+24]) attribute "ns.namedArgLoc" (test.res[15,209+11]..[15,209+16]) []
   Pexp_constant PConst_string ("hello",Some "js") <arg> Labelled "children" expression
   (test.res[15,209+25]..[15,209+26]) Pexp_construct "[]" (test.res[15,209+25]..[15,209+26]) ghost None <arg>
   Nolabel expression (_none_[1,0+-1]..[1,0+-1]) ghost Pexp_construct "()" (_none_[1,0+-1]..[1,0+-1]) ghost
   None ] expression (test.res[14,201+6]..[16,237+2]) ghost Pexp_construct "[]"
   (test.res[14,201+6]..[16,237+2]) ghost None ] <arg> Nolabel expression (_none_[1,0+-1]..[1,0+-1]) ghost
   Pexp_construct "()" (_none_[1,0+-1]..[1,0+-1]) ghost None ] ] ] *)
