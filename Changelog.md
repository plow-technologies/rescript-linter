# ReScript Linter Changelog

### 2025-09-23 - v0.3.1
* Added a `DisallowDeadCode` rule to disallow code tagged with @dead
* Added the following as iteration targets
  * LintTypeKind
  * LintLabelDeclaration
  * LintConstructorDeclaration
  * LintValueBinding
  * LintTypeDeclaration
  * LintModuleBinding
  * LintClassTypeDeclaration

### 2024-10-16 - v0.3.0
* Vendored the compiler subdirector of the rescript compiler repo to support newer syntax within the rescript language. This will allow for better parsing of the AST and better linting going forward
  * However, this does mean that linting of the `|>` is no longer supported as the AST no longer has the triangle pipe explicitly in the AST
  * Also linting support for J String interpolation is no longer supported as the compiler will now warn on this syntax.
* Changed the nix build to use flakes

### 2024-10-11 - v0.2.2
* Added a new rule for disallowing regex literals in functions.
  * With the rule enabled the literals must be assigned to a variable and then the variable must have a test specified somewhere in the specified directory in the config file.

### 2022-09-19 - v0.2.1
* Add suggested_component for NoReactComponent rule

### 2022-08-05 - v0.2.0
* Redesign how AST is parsed to each rule (allow for multiple AST roots)
* New rule: DisallowModuleRule

### 2022-08-02 - v0.1.0
* Better support for disabling lint
* New rule: NoReactComponent

### 2022-08-02 - v0.0.3
* Disable lint with comment

### 2022-08-01 - v0.0.2
* Read lint rules from config file
