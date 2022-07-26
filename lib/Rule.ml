open Rescript_parser

type _ modifier =
  | MExpression : Parsetree.expression modifier
  | MStructure : Parsetree.structure modifier

type lintResult =
  | LintError of string * Location.t
  | LintOk

type meta =
  { ruleName : string
  ; ruleDescription : string
  }

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

(*
module type HasRule2 = sig
  type t
  val lint : t -> lintResult
end

module type ISRULE = sig
  type t
  val proxy : t modifier
  val lint : t -> lintResult
end

module MakeRule2 : ISRULE  = struct
  type t = Parsetree.expression
  let proxy = MExpression
  let lint _expr = LintOk
end
*)
