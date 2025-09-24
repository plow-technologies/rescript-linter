type lintResult = LintError of string * Location.t | LintOk

type linter =
  | LintExpression of (Parsetree.expression -> lintResult)
  | LintStructure of (Parsetree.structure -> lintResult)
  | LintStructureItem of (Parsetree.structure_item -> lintResult)
  | LintPattern of (Parsetree.pattern -> lintResult)
  | LintTypeKind of (Parsetree.type_kind -> lintResult)
  | LintLabelDeclaration of (Parsetree.label_declaration -> lintResult)
  | LintConstructorDeclaration of (Parsetree.constructor_declaration -> lintResult)
  | LintValueBinding of (Parsetree.value_binding -> lintResult)
  | LintTypeDeclaration of (Parsetree.type_declaration -> lintResult)
  | LintModuleBinding of (Parsetree.module_binding -> lintResult)
  | LintClassTypeDeclaration of (Parsetree.class_type_declaration -> lintResult)

(* These are additional options that are parsed along side the rule name and the rule-specific options
 * These rules are common to all rules and can be used to modify the behavior of the rule
 *)
module type LinterOptions = sig
  (* Whether to show warnings for this rule - when false, or not specified, the rule will report as an error *)
  val warning : bool
end

type meta = {ruleIdentifier: string; ruleName: string; ruleDescription: string}

let meta_to_string meta =
  Printf.sprintf "{ ruleIdentifier: %s; ruleName: %s; ruleDescription: %s }" meta.ruleIdentifier meta.ruleName
    meta.ruleDescription

module type HASRULE = sig
  val meta : meta

  val warning : bool

  val linters : linter list
end

module type OPTIONS = sig
  type options

  val options : options
end
