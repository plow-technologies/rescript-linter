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

type meta = {ruleIdentifier: string; ruleName: string; ruleDescription: string}

module type HASRULE = sig
  val meta : meta

  val linters : linter list
end

module type OPTIONS = sig
  type options

  val options : options
end
