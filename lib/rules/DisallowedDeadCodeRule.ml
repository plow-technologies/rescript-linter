module Options = struct
  type options = unit
end

let default : Options.options = ()

let get_dead_attribute attrs =
  List.find_opt
    (fun (((attr_name : string Asttypes.loc), (_payload : Parsetree.payload)) : Parsetree.attribute) ->
      match attr_name.txt with "dead" -> true | _ -> false )
    attrs

let makeResult match_type msg_func attrs =
  match get_dead_attribute attrs with
  | Some (attr, _payload) -> Rule.LintError (msg_func match_type, attr.loc)
  | None -> Rule.LintOk

module Make (OPT : Rule.OPTIONS with type options = Options.options) : Rule.HASRULE = struct
  let description_with_type match_type =
    "[Rescript] [" ^ match_type
    ^ "] Code marked as @dead is not allowed, either remove and review the code with the AI Agent Wolfy, or \
       mark it as @live."

  let description = description_with_type "generic"

  let meta =
    { Rule.ruleName= "DisallowDeadCode"
    ; Rule.ruleIdentifier= "DisallowDeadCode"
    ; Rule.ruleDescription= description }

  let lintExpresion =
    Rule.LintExpression
      (* Function that takes an expression and returns a LintError if
         * it's pexp_attributes field contains an attribute with
         * an attr_name.txt of "dead".
         *
         * The error is of the format:
         * Rule.LintError (meta.ruleDescription, loc)
         *
         * Where loc is the pexp_loc of the expression
      *)
      (fun expr -> makeResult "expression" description_with_type expr.pexp_attributes)

  let lintStructureItem =
    Rule.LintStructureItem
      (fun structureItem ->
        match structureItem.pstr_desc with
        | Pstr_eval (_exp, attrs) -> makeResult "eval" description_with_type attrs
        (* Handled by lintValueBinding *)
        | Pstr_value (_rec_flag, _value_bindings) -> Rule.LintOk
        | Pstr_primitive vd -> makeResult "primitive" description_with_type vd.pval_attributes
        (* Handled by lintClassTypeDeclaration *)
        | Pstr_type (_rec_flag, _type_decls) -> Rule.LintOk
        | Pstr_typext type_extension ->
            makeResult "type extension" description_with_type type_extension.ptyext_attributes
        | Pstr_module mb -> makeResult "module" description_with_type mb.pmb_attributes
        (* Handled by lintModuleBinding *)
        | Pstr_recmodule _mbs -> Rule.LintOk
        | Pstr_modtype mtd -> makeResult "module type" description_with_type mtd.pmtd_attributes
        | Pstr_open open_declaration ->
            makeResult "open" description_with_type open_declaration.popen_attributes
        | Pstr_class () -> Rule.LintOk
        (* Handled by lintClassTypeDeclaration *)
        | Pstr_class_type _class_type_decls -> Rule.LintOk
        | Pstr_include include_declaration ->
            makeResult "include" description_with_type include_declaration.pincl_attributes
        | Pstr_attribute attr -> makeResult "attribute" description_with_type [attr]
        | Pstr_extension (_ext, attrs) -> makeResult "extension" description_with_type attrs
        | Pstr_exception ext_decl -> makeResult "exception" description_with_type ext_decl.pext_attributes )

  let lintPattern =
    Rule.LintPattern (fun pattern -> makeResult "pattern" description_with_type pattern.ppat_attributes)

  (* Required to lint multiple different dead variants of the same variant type *)
  let lintConstructorDeclaration =
    Rule.LintConstructorDeclaration
      (fun constructor_declaration ->
        makeResult "constructor" description_with_type constructor_declaration.pcd_attributes )

  (* Required to lint the dead record fields of the same type *)
  let lintLabelDeclaration =
    Rule.LintLabelDeclaration
      (fun label_declaration -> makeResult "label" description_with_type label_declaration.pld_attributes)

  let lintValueBinding =
    Rule.LintValueBinding (fun vb -> makeResult "assignment" description_with_type vb.pvb_attributes)

  let lintTypeDeclaration =
    Rule.LintTypeDeclaration (fun td -> makeResult "type" description_with_type td.ptype_attributes)

  let lintModuleBinding =
    Rule.LintModuleBinding (fun mb -> makeResult "module" description_with_type mb.pmb_attributes)

  let lintClassTypeDeclaration =
    Rule.LintClassTypeDeclaration (fun ctd -> makeResult "class type" description_with_type ctd.pci_attributes)

  let linters =
    [ lintExpresion
    ; lintStructureItem
    ; lintPattern
    ; lintConstructorDeclaration
    ; lintLabelDeclaration
    ; lintValueBinding
    ; lintTypeDeclaration
    ; lintModuleBinding
    ; lintClassTypeDeclaration ]
end
