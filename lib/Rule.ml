type lintResult = LintError of string * Location.t | LintOk

type linter =
  | LintExpression of (Parsetree.expression -> lintResult)
  | LintStructure of (Parsetree.structure -> lintResult)
  | LintStructureItem of (Parsetree.structure_item -> lintResult)
  | LintPattern of (Parsetree.pattern -> lintResult)

type meta = {ruleIdentifier: string; ruleName: string; ruleDescription: string}

module type HASRULE = sig
  val meta : meta

  val linters : linter list
end

module type OPTIONS = sig
  type options

  val options : options
end
