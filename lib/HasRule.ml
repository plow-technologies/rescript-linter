open Rescript_parser

type lintResult =
  | LintError of string
  | LintOk

type meta =
  { ruleName : string
  ; ruleDescription : string
  }

module type HasRule = sig
  val meta : meta
  val lint : Parsetree.structure -> unit
end

module type IsRule = sig
  val meta : meta
  val lint : Parsetree.structure -> unit
end

module MakeRule  (Rule : HasRule) : IsRule = struct
  let meta = Rule.meta
  let lint _ = ()
end
