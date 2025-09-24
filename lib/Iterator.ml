let withPattern iterator f callback =
  { iterator with
    Ast_iterator.pat=
      (fun iterator1 pattern ->
        let res = f pattern in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.pat iterator1 pattern ) }

let withStructure iterator f callback =
  { iterator with
    Ast_iterator.structure=
      (fun iterator1 structure ->
        let res = f structure in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.structure iterator1 structure ) }

let withStructureItem iterator f callback =
  { iterator with
    Ast_iterator.structure_item=
      (fun iterator1 structure ->
        let res = f structure in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.structure_item iterator1 structure ) }

let withExpression iterator f callback =
  { iterator with
    Ast_iterator.expr=
      (fun iterator1 expr ->
        let res = f expr in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.expr iterator1 expr ) }

let withTypeKind iterator f callback =
  { iterator with
    Ast_iterator.type_kind=
      (fun iterator1 type_kind ->
        let res = f type_kind in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.type_kind iterator1 type_kind ) }

let withLabelDeclaration iterator f callback =
  { iterator with
    Ast_iterator.label_declaration=
      (fun iterator1 label_declaration ->
        let res = f label_declaration in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.label_declaration iterator1 label_declaration ) }

let withConstructorDeclaration iterator f callbackn =
  { iterator with
    Ast_iterator.constructor_declaration=
      (fun iterator1 constructor_declaration ->
        let res = f constructor_declaration in
        (match res with Rule.LintError (msg, loc) -> callbackn (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.constructor_declaration iterator1 constructor_declaration ) }

let withValueBinding iterator f callback =
  { iterator with
    Ast_iterator.value_binding=
      (fun iterator1 value_binding ->
        let res = f value_binding in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.value_binding iterator1 value_binding ) }

let withTypeDeclaration iterator f callback =
  { iterator with
    Ast_iterator.type_declaration=
      (fun iterator1 type_declaration ->
        let res = f type_declaration in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.type_declaration iterator1 type_declaration ) }

let withModuleBinding iterator f callback =
  { iterator with
    Ast_iterator.module_binding=
      (fun iterator1 module_binding ->
        let res = f module_binding in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.module_binding iterator1 module_binding ) }

let withClassTypeDeclaration iterator f callback =
  { iterator with
    Ast_iterator.class_type_declaration=
      (fun iterator1 class_type_declaration ->
        let res = f class_type_declaration in
        (match res with Rule.LintError (msg, loc) -> callback (msg, loc) | Rule.LintOk -> ()) ;
        iterator.Ast_iterator.class_type_declaration iterator1 class_type_declaration ) }

(* Callbacks for linting errors vs warnings *)
type callbacks = {errorCallback: string * Location.t -> unit; warningCallback: string * Location.t -> unit}

let makeIterator rules callbacks =
  let f iterator rule =
    let module R = (val rule : Rule.HASRULE) in
    (* Either warn or error, based on the linter options copied to the rule module *)
    let callback = if R.warning then callbacks.warningCallback else callbacks.errorCallback in
    let buildIterator iterator lint =
      match lint with
      | Rule.LintExpression lintFunc -> withExpression iterator lintFunc callback
      | Rule.LintStructure lintFunc -> withStructure iterator lintFunc callback
      | Rule.LintStructureItem lintFunc -> withStructureItem iterator lintFunc callback
      | Rule.LintPattern lintFunc -> withPattern iterator lintFunc callback
      | Rule.LintTypeKind lintFunc -> withTypeKind iterator lintFunc callback
      | Rule.LintLabelDeclaration lintFunc -> withLabelDeclaration iterator lintFunc callback
      | Rule.LintConstructorDeclaration lintFunc -> withConstructorDeclaration iterator lintFunc callback
      | Rule.LintValueBinding lintFunc -> withValueBinding iterator lintFunc callback
      | Rule.LintTypeDeclaration lintFunc -> withTypeDeclaration iterator lintFunc callback
      | Rule.LintModuleBinding lintFunc -> withModuleBinding iterator lintFunc callback
      | Rule.LintClassTypeDeclaration lintFunc -> withClassTypeDeclaration iterator lintFunc callback
    in
    List.fold_left buildIterator iterator R.linters
  in
  List.fold_left f Ast_iterator.default_iterator rules
