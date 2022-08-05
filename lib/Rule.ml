open Rescript_parser

type _ modifier =
  | MExpression : Parsetree.expression modifier
  | MStructure : Parsetree.structure modifier
  | MStructureItem : Parsetree.structure_item modifier
  | MPattern : Parsetree.pattern modifier

type lintResult = LintError of string * Location.t | LintOk

type meta = {ruleIdentifier: string; ruleName: string; ruleDescription: string}

module type HASRULE = sig
  type t

  val proxy : t modifier

  val meta : meta

  val lint : t -> lintResult
end

module type OPTIONS = sig
  type options

  val options : options
end
