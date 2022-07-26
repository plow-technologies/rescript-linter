open Rescript_parser

type _ modifier =
  | MExpression : Parsetree.expression modifier
  | MStructure : Parsetree.structure modifier

type lintResult =
  | LintError of string
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

module type IsRule = sig
  val meta : meta
  val lint : 'a -> lintResult
end

module Make (R : HASRULE) : HASRULE = struct
  type t = R.t
  let proxy = R.proxy
  let meta = R.meta
  let lint = R.lint
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
