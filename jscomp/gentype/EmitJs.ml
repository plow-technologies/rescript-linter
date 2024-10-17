open GenTypeCommon

type env = {
  requiresEarly: ImportPath.t Config.ModuleNameMap.t;
  requires: ImportPath.t Config.ModuleNameMap.t;
      (**  For each .cmt we import types from, keep the map of exported types *)
  cmtToExportTypeMap: CodeItem.exportTypeMap StringMap.t;
      (**  Map of types imported from other files *)
  exportTypeMapFromOtherFiles: CodeItem.exportTypeMap;
  importedValueOrComponent: bool;
}

let requireModule ~import ~env ~importPath moduleName =
  let requires =
    match import with
    | true -> env.requiresEarly
    | false -> env.requires
  in
  let requiresNew =
    requires |> Config.ModuleNameMap.add moduleName importPath
  in
  match import with
  | true -> {env with requiresEarly = requiresNew}
  | false -> {env with requires = requiresNew}

let createExportTypeMap ~config ~file ~fromCmtReadRecursively
    (typeDeclarations : CodeItem.typeDeclaration list) : CodeItem.exportTypeMap
    =
  if !Debug.codeItems then Log_.item "Create Type Map for %s\n" file;
  let updateExportTypeMap (exportTypeMap : CodeItem.exportTypeMap)
      (typeDeclaration : CodeItem.typeDeclaration) : CodeItem.exportTypeMap =
    let addExportType ~annotation
        ({resolvedTypeName; type_; typeVars} : CodeItem.exportType) =
      let annotation =
        match annotation with
        | Annotation.NoGenType when fromCmtReadRecursively -> Annotation.GenType
        | _ -> annotation
      in
      if !Debug.codeItems then
        Log_.item "Type Map: %s%s%s\n"
          (resolvedTypeName |> ResolvedName.toString)
          (match typeVars = [] with
          | true -> ""
          | false -> "(" ^ (typeVars |> String.concat ",") ^ ")")
          (" "
          ^ (annotation |> Annotation.toString |> EmitText.comment)
          ^ " = "
          ^ (type_
            |> EmitType.typeToString ~config ~typeNameIsInterface:(fun _ ->
                   false)));
      exportTypeMap
      |> StringMap.add
           (resolvedTypeName |> ResolvedName.toString)
           {CodeItem.typeVars; type_; annotation}
    in
    match typeDeclaration.exportFromTypeDeclaration with
    | {exportType; annotation} -> exportType |> addExportType ~annotation
  in
  typeDeclarations |> List.fold_left updateExportTypeMap StringMap.empty

let codeItemToString ~config ~typeNameIsInterface (codeItem : CodeItem.t) =
  match codeItem with
  | ExportValue {resolvedName; type_} ->
    "ExportValue" ^ " resolvedName:"
    ^ ResolvedName.toString resolvedName
    ^ " type:"
    ^ EmitType.typeToString ~config ~typeNameIsInterface type_
  | ImportValue {importAnnotation} ->
    "ImportValue " ^ (importAnnotation.importPath |> ImportPath.dump)

let emitExportType ~emitters ~config ~typeNameIsInterface
    {CodeItem.loc; nameAs; opaque; type_; typeVars; resolvedTypeName; docString}
    =
  let freeTypeVars = TypeVars.free type_ in
  let isGADT =
    freeTypeVars |> List.exists (fun s -> not (List.mem s typeVars))
  in
  let opaque =
    match opaque with
    | Some true -> opaque
    | _ when isGADT ->
      Log_.Color.setup ();
      Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
          Format.fprintf ppf
            "GADT types are not supported: exporting %s as opaque type"
            (resolvedTypeName |> ResolvedName.toString));
      Some true
    | _ -> opaque
  in
  let opaque =
    match opaque with
    | Some opaque -> opaque
    | None -> false
  in
  resolvedTypeName |> ResolvedName.toString
  |> EmitType.emitExportType ~config ~emitters ~nameAs ~opaque ~type_
       ~typeNameIsInterface ~typeVars ~docString

let typeNameIsInterface ~(exportTypeMap : CodeItem.exportTypeMap)
    ~(exportTypeMapFromOtherFiles : CodeItem.exportTypeMap) typeName =
  let typeIsInterface type_ =
    match type_ with
    | Object _ -> true
    | _ -> false
  in
  match exportTypeMap |> StringMap.find typeName with
  | {type_} -> type_ |> typeIsInterface
  | exception Not_found -> (
    match exportTypeMapFromOtherFiles |> StringMap.find typeName with
    | {type_} -> type_ |> typeIsInterface
    | exception Not_found -> false)

let emitExportFromTypeDeclaration ~config ~emitters ~env ~typeNameIsInterface
    (exportFromTypeDeclaration : CodeItem.exportFromTypeDeclaration) =
  ( env,
    exportFromTypeDeclaration.exportType
    |> emitExportType ~emitters ~config ~typeNameIsInterface )

let emitExportFromTypeDeclarations ~config ~emitters ~env ~typeNameIsInterface
    exportFromTypeDeclarations =
  exportFromTypeDeclarations
  |> List.fold_left
       (fun (env, emitters) ->
         emitExportFromTypeDeclaration ~config ~emitters ~env
           ~typeNameIsInterface)
       (env, emitters)

let emitCodeItem ~config ~emitters ~moduleItemsEmitter ~env ~fileName
    ~outputFileRelative ~resolver ~inlineOneLevel ~typeNameIsInterface codeItem
    =
  if !Debug.codeItems then
    Log_.item "Code Item: %s\n"
      (codeItem |> codeItemToString ~config ~typeNameIsInterface);
  match codeItem with
  | ImportValue {asPath; importAnnotation; type_; valueName} ->
    let importPath = importAnnotation.importPath in
    let firstNameInPath, restOfPath =
      match valueName = asPath with
      | true -> (valueName, "")
      | false -> (
        match asPath |> String.split_on_char '.' with
        | x :: y -> (x, "" :: y |> String.concat ".")
        | _ -> (asPath, ""))
    in
    let emitters, importedAsName, env =
      (* emit an import {... as ...} immediately *)
      let valueNameNotChecked = valueName ^ "NotChecked" in
      let emitters =
        importPath
        |> EmitType.emitImportValueAsEarly ~emitters ~name:firstNameInPath
             ~nameAs:(Some valueNameNotChecked)
      in
      (emitters, valueNameNotChecked, env)
    in
    let type_ =
      match type_ with
      | Function
          ({argTypes = [{aType = Object (closedFlag, fields); aName}]; retType}
           as function_)
        when retType |> EmitType.isTypeFunctionComponent ~fields ->
        (* JSX V3 *)
        let fields =
          fields
          |> List.map (fun (field : field) ->
                 match
                   field.nameJS = "children"
                   && field.type_ |> EmitType.isTypeReactElement
                 with
                 | true -> {field with type_ = EmitType.typeReactChild}
                 | false -> field)
        in
        let function_ =
          {
            function_ with
            argTypes = [{aType = Object (closedFlag, fields); aName}];
          }
        in
        Function function_
      | Function
          ({argTypes = [{aType = Ident {name} as propsType; aName}]; retType} as
           function_)
        when Filename.check_suffix name "props"
             && retType |> EmitType.isTypeFunctionComponent ~fields:[] -> (
        match inlineOneLevel propsType with
        | Object (closedFlags, fields) ->
          (* JSX V3 *)
          let fields =
            Ext_list.filter_map fields (fun (field : field) ->
                match field.nameJS with
                | "children" when field.type_ |> EmitType.isTypeReactElement ->
                  Some {field with type_ = EmitType.typeReactChild}
                | "key" ->
                  (* Filter out key, which is added to the props type definition in V4 *)
                  None
                | _ -> Some field)
          in
          let function_ =
            {
              function_ with
              argTypes = [{aType = Object (closedFlags, fields); aName}];
            }
          in
          Function function_
        | _ -> type_)
      | _ -> type_
    in
    let valueNameTypeChecked = valueName ^ "TypeChecked" in
    let emitters =
      importedAsName ^ restOfPath
      |> EmitType.emitExportConst ~config
           ~comment:
             ("In case of type error, check the type of '" ^ valueName
            ^ "' in '"
             ^ (fileName |> ModuleName.toString)
             ^ ".res'" ^ " and '"
             ^ (importPath |> ImportPath.emit)
             ^ "'.")
           ~early:true ~emitters ~name:valueNameTypeChecked ~type_
           ~typeNameIsInterface
    in
    let valueNameNotDefault =
      match valueName = "default" with
      | true -> Runtime.default
      | false -> valueName
    in
    let emitters =
      valueNameTypeChecked
      |> EmitType.emitTypeCast ~config ~type_ ~typeNameIsInterface
      |> EmitType.emitExportConst
           ~comment:
             ("Export '" ^ valueNameNotDefault
            ^ "' early to allow circular import from the '.bs.js' file.")
           ~config ~early:true ~emitters ~name:valueNameNotDefault
           ~type_:unknown ~typeNameIsInterface
    in
    let emitters =
      match valueName = "default" with
      | true -> EmitType.emitExportDefault ~emitters valueNameNotDefault
      | false -> emitters
    in
    ({env with importedValueOrComponent = true}, emitters)
  | ExportValue {docString; moduleAccessPath; originalName; resolvedName; type_}
    ->
    let resolvedNameStr = ResolvedName.toString resolvedName in
    let importPath =
      fileName
      |> ModuleResolver.resolveModule ~config ~importExtension:config.suffix
           ~outputFileRelative ~resolver ~useBsDependencies:false
    in
    let fileNameJs = fileName |> ModuleName.forJsFile in
    let envWithRequires =
      fileNameJs |> requireModule ~import:false ~env ~importPath
    in
    let default = "default" in
    let make = "make" in
    let name =
      match originalName = default with
      | true -> Runtime.default
      | false -> resolvedNameStr
    in
    let module HookType = struct
      type t = {
        propsType: type_;
        resolvedTypeName: ResolvedName.t;
        typeVars: string list;
      }
    end in
    let type_, hookType =
      match type_ with
      | Function
          ({
             argTypes = [{aType = Object (closedFlags, fields)}];
             retType;
             typeVars;
           } as function_)
        when retType |> EmitType.isTypeFunctionComponent ~fields ->
        (* JSX V3 *)
        let propsType =
          let fields =
            fields
            |> List.map (fun (field : field) ->
                   match
                     field.nameJS = "children"
                     && field.type_ |> EmitType.isTypeReactElement
                   with
                   | true -> {field with type_ = EmitType.typeReactChild}
                   | false -> field)
          in
          Object (closedFlags, fields)
        in
        let function_ =
          {function_ with argTypes = [{aName = ""; aType = propsType}]}
        in
        let resolvedTypeName =
          if
            (not config.emitTypePropDone)
            && (originalName = default || originalName = make)
          then (
            config.emitTypePropDone <- true;
            ResolvedName.fromString "Props")
          else ResolvedName.fromString name |> ResolvedName.dot "Props"
        in
        ( Function function_,
          Some {HookType.propsType; resolvedTypeName; typeVars} )
      | Function
          ({argTypes = [{aType = Ident {name} as propsType}]; retType} as
           function_)
        when Filename.check_suffix name "props"
             && retType |> EmitType.isTypeFunctionComponent ~fields:[] ->
        let compType =
          match inlineOneLevel propsType with
          | Object (closedFlags, fields) ->
            (* JSX V4 *)
            let propsType =
              let fields =
                Ext_list.filter_map fields (fun (field : field) ->
                    match field.nameJS with
                    | "children" when field.type_ |> EmitType.isTypeReactElement
                      ->
                      Some {field with type_ = EmitType.typeReactChild}
                    | "key" ->
                      (* Filter out key, which is added to the props type definition in V4 *)
                      None
                    | _ -> Some field)
              in
              Object (closedFlags, fields)
            in
            let function_ =
              {function_ with argTypes = [{aName = ""; aType = propsType}]}
            in
            Function function_
          | _ -> type_
        in
        (compType, None)
      | _ -> (type_, None)
    in

    resolvedName
    |> ExportModule.extendExportModules ~docString ~moduleItemsEmitter ~type_;
    let emitters =
      match hookType with
      | Some {propsType; resolvedTypeName; typeVars} ->
        let exportType =
          ({
             loc = Location.none;
             nameAs = None;
             opaque = Some false;
             type_ = propsType;
             typeVars;
             resolvedTypeName;
             docString;
           }
            : CodeItem.exportType)
        in
        (* For doc gen (https://github.com/cristianoc/genType/issues/342) *)
        config.emitImportReact <- true;
        emitExportType ~emitters ~config ~typeNameIsInterface exportType
      | _ -> emitters
    in
    let emitters =
      (fileNameJs |> ModuleName.toString)
      ^ "."
      ^ (moduleAccessPath |> Runtime.emitModuleAccessPath ~config)
      |> EmitType.emitExportConst ~config ~docString ~early:false ~emitters
           ~name ~type_ ~typeNameIsInterface
    in
    let emitters =
      match originalName = default with
      | true -> EmitType.emitExportDefault ~emitters Runtime.default
      | false -> emitters
    in
    (envWithRequires, emitters)

let emitCodeItems ~config ~outputFileRelative ~emitters ~moduleItemsEmitter ~env
    ~fileName ~resolver ~typeNameIsInterface ~inlineOneLevel codeItems =
  codeItems
  |> List.fold_left
       (fun (env, emitters) ->
         emitCodeItem ~config ~emitters ~moduleItemsEmitter ~env ~fileName
           ~outputFileRelative ~resolver ~inlineOneLevel ~typeNameIsInterface)
       (env, emitters)

let emitRequires ~importedValueOrComponent ~early ~config ~requires emitters =
  Config.ModuleNameMap.fold
    (fun moduleName importPath emitters ->
      importPath
      |> EmitType.emitRequire ~importedValueOrComponent ~early ~emitters ~config
           ~moduleName)
    requires emitters

let typeGetInlined ~config ~exportTypeMap type_ =
  type_
  |> Converter.typeGetInlined ~config
       ~lookupId:(fun s -> exportTypeMap |> StringMap.find s)
       ~typeNameIsInterface:(fun _ -> false)

(** Read the cmt file referenced in an import type,
   and recursively for the import types obtained from reading the cmt file. *)
let rec readCmtFilesRecursively ~config ~env ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver {CodeItem.typeName; asTypeName; importPath} =
  let updateTypeMapFromOtherFiles ~asType ~exportTypeMapFromCmt env =
    match exportTypeMapFromCmt |> StringMap.find typeName with
    | (exportTypeItem : CodeItem.exportTypeItem) ->
      let type_ =
        exportTypeItem.type_
        |> typeGetInlined ~config ~exportTypeMap:exportTypeMapFromCmt
      in
      {
        env with
        exportTypeMapFromOtherFiles =
          env.exportTypeMapFromOtherFiles
          |> StringMap.add asType {exportTypeItem with type_};
      }
    | exception Not_found -> env
  in
  let cmtFile =
    importPath
    |> ImportPath.toCmt ~config ~outputFileRelative
    |> Paths.getCmtFile
  in
  match asTypeName with
  | Some asType when cmtFile <> "" -> (
    match env.cmtToExportTypeMap |> StringMap.find cmtFile with
    | exportTypeMapFromCmt ->
      env |> updateTypeMapFromOtherFiles ~asType ~exportTypeMapFromCmt
    | exception Not_found ->
      (* cmt file not read before: this ensures termination  *)
      let typeDeclarations =
        Cmt_format.read_cmt cmtFile
        |> inputCmtTranslateTypeDeclarations ~config ~outputFileRelative
             ~resolver
        |> fun (x : CodeItem.translation) -> x.typeDeclarations
      in
      let exportTypeMapFromCmt =
        typeDeclarations
        |> createExportTypeMap ~config ~fromCmtReadRecursively:true
             ~file:
               (cmtFile |> Filename.basename
              |> (Filename.chop_extension [@doesNotRaise]))
      in
      let cmtToExportTypeMap =
        env.cmtToExportTypeMap |> StringMap.add cmtFile exportTypeMapFromCmt
      in
      let env =
        {env with cmtToExportTypeMap}
        |> updateTypeMapFromOtherFiles ~asType ~exportTypeMapFromCmt
      in
      let newImportTypes =
        typeDeclarations
        |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
               typeDeclaration.importTypes)
        |> List.concat
      in
      newImportTypes
      |> List.fold_left
           (fun env newImportType ->
             newImportType
             |> readCmtFilesRecursively ~config ~env
                  ~inputCmtTranslateTypeDeclarations ~outputFileRelative
                  ~resolver)
           env)
  | _ -> env

let emitImportType ~config ~emitters ~env ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver ~typeNameIsInterface
    ({CodeItem.typeName; asTypeName; importPath} as importType) =
  let env =
    importType
    |> readCmtFilesRecursively ~config ~env ~inputCmtTranslateTypeDeclarations
         ~outputFileRelative ~resolver
  in
  let emitters =
    EmitType.emitImportTypeAs ~emitters ~config ~typeName ~asTypeName
      ~typeNameIsInterface:(typeNameIsInterface ~env) ~importPath
  in
  (env, emitters)

let emitImportTypes ~config ~emitters ~env ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver ~typeNameIsInterface importTypes =
  importTypes
  |> List.fold_left
       (fun (env, emitters) ->
         emitImportType ~config ~emitters ~env
           ~inputCmtTranslateTypeDeclarations ~outputFileRelative ~resolver
           ~typeNameIsInterface)
       (env, emitters)

let getAnnotatedTypedDeclarations ~annotatedSet typeDeclarations =
  typeDeclarations
  |> List.map (fun typeDeclaration ->
         let nameInAnnotatedSet =
           annotatedSet
           |> StringSet.mem
                (typeDeclaration.CodeItem.exportFromTypeDeclaration.exportType
                   .resolvedTypeName |> ResolvedName.toString)
         in
         if nameInAnnotatedSet then
           {
             typeDeclaration with
             exportFromTypeDeclaration =
               {
                 typeDeclaration.exportFromTypeDeclaration with
                 annotation = GenType;
               };
           }
         else typeDeclaration)
  |> List.filter
       (fun
         ({exportFromTypeDeclaration = {annotation}} : CodeItem.typeDeclaration)
       -> annotation <> NoGenType)

let propagateAnnotationToSubTypes ~codeItems (typeMap : CodeItem.exportTypeMap)
    =
  let annotatedSet = ref StringSet.empty in
  let initialAnnotatedTypes =
    typeMap |> StringMap.bindings
    |> List.filter (fun (_, {CodeItem.annotation}) ->
           annotation = Annotation.GenType)
    |> List.map (fun (_, {CodeItem.type_}) -> type_)
  in
  let typesOfExportedValue (codeItem : CodeItem.t) =
    match codeItem with
    | ExportValue {type_} | ImportValue {type_} -> [type_]
  in
  let typesOfExportedValues =
    codeItems |> List.map typesOfExportedValue |> List.concat
  in
  let visitTypAndUpdateMarked type0 =
    let visited = ref StringSet.empty in
    let rec visit type_ =
      match type_ with
      | Ident {name = typeName; typeArgs} ->
        if !visited |> StringSet.mem typeName then ()
        else (
          visited := !visited |> StringSet.add typeName;
          typeArgs |> List.iter visit;
          match typeMap |> StringMap.find typeName with
          | {annotation = GenType | GenTypeOpaque} -> ()
          | {type_ = type1; annotation = NoGenType} ->
            if !Debug.translation then
              Log_.item "Marking Type As Annotated %s\n" typeName;
            annotatedSet := !annotatedSet |> StringSet.add typeName;
            type1 |> visit
          | exception Not_found ->
            annotatedSet := !annotatedSet |> StringSet.add typeName)
      | Array (t, _) | Dict t -> t |> visit
      | Function {argTypes; retType} ->
        argTypes |> List.iter (fun {aType} -> visit aType);
        retType |> visit
      | Object (_, fields) ->
        fields |> List.iter (fun {type_} -> type_ |> visit)
      | Option t | Null t | Nullable t | Promise t -> t |> visit
      | Tuple innerTypes -> innerTypes |> List.iter visit
      | TypeVar _ -> ()
      | Variant {inherits; payloads} ->
        inherits |> List.iter visit;
        payloads |> List.iter (fun {t} -> t |> visit)
    in
    type0 |> visit
  in
  initialAnnotatedTypes @ typesOfExportedValues
  |> List.iter visitTypAndUpdateMarked;
  let newTypeMap =
    typeMap
    |> StringMap.mapi
         (fun typeName (exportTypeItem : CodeItem.exportTypeItem) ->
           {
             exportTypeItem with
             annotation =
               (match !annotatedSet |> StringSet.mem typeName with
               | true -> Annotation.GenType
               | false -> exportTypeItem.annotation);
           })
  in
  (newTypeMap, !annotatedSet)

let emitTranslationAsString ~config ~fileName ~inputCmtTranslateTypeDeclarations
    ~outputFileRelative ~resolver (translation : Translation.t) =
  let initialEnv =
    {
      requires = Config.ModuleNameMap.empty;
      requiresEarly = Config.ModuleNameMap.empty;
      cmtToExportTypeMap = StringMap.empty;
      exportTypeMapFromOtherFiles = StringMap.empty;
      importedValueOrComponent = false;
    }
  in
  let exportTypeMap, annotatedSet =
    translation.typeDeclarations
    |> createExportTypeMap ~config
         ~file:(fileName |> ModuleName.toString)
         ~fromCmtReadRecursively:false
    |> propagateAnnotationToSubTypes ~codeItems:translation.codeItems
  in
  let annotatedTypeDeclarations =
    translation.typeDeclarations |> getAnnotatedTypedDeclarations ~annotatedSet
  in
  let importTypesFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
           typeDeclaration.importTypes)
    |> List.concat
  in
  let exportFromTypeDeclarations =
    annotatedTypeDeclarations
    |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
           typeDeclaration.exportFromTypeDeclaration)
  in
  let typeNameIsInterface ~env =
    typeNameIsInterface ~exportTypeMap
      ~exportTypeMapFromOtherFiles:env.exportTypeMapFromOtherFiles
  in
  let lookupId_ ~env s =
    try exportTypeMap |> StringMap.find s
    with Not_found -> env.exportTypeMapFromOtherFiles |> StringMap.find s
  in
  let emitters = Emitters.initial
  and moduleItemsEmitter = ExportModule.createModuleItemsEmitter ()
  and env = initialEnv in
  let env, emitters =
    (* imports from type declarations go first to build up type tables *)
    importTypesFromTypeDeclarations @ translation.importTypes
    |> List.sort_uniq Translation.importTypeCompare
    |> emitImportTypes ~config ~emitters ~env ~inputCmtTranslateTypeDeclarations
         ~outputFileRelative ~resolver ~typeNameIsInterface
  in
  let env, emitters =
    exportFromTypeDeclarations
    |> emitExportFromTypeDeclarations ~config ~emitters ~env
         ~typeNameIsInterface:(typeNameIsInterface ~env)
  in
  let inlineOneLevel type_ =
    match type_ with
    | Ident {builtin = false; name; typeArgs} -> (
      match name |> lookupId_ ~env with
      | {type_; typeVars} ->
        let pairs =
          try List.combine typeVars typeArgs with Invalid_argument _ -> []
        in
        let f typeVar =
          match
            pairs |> List.find (fun (typeVar1, _) -> typeVar = typeVar1)
          with
          | _, typeArgument -> Some typeArgument
          | exception Not_found -> None
        in
        type_ |> TypeVars.substitute ~f
      | exception Not_found -> type_)
    | _ -> type_
  in
  let env, emitters =
    translation.codeItems
    |> emitCodeItems ~config ~emitters ~moduleItemsEmitter ~env ~fileName
         ~outputFileRelative ~resolver ~inlineOneLevel
         ~typeNameIsInterface:(typeNameIsInterface ~env)
  in
  let emitters =
    match config.emitImportReact with
    | true -> EmitType.emitImportReact ~emitters
    | false -> emitters
  in
  let env =
    match config.emitImportCurry with
    | true ->
      ModuleName.curry
      |> requireModule ~import:true ~env
           ~importPath:(ImportPath.bsCurryPath ~config)
    | false -> env
  in
  let finalEnv = env in
  let emitters =
    moduleItemsEmitter
    |> ExportModule.emitAllModuleItems ~config ~emitters ~fileName
  in
  emitters
  |> emitRequires ~importedValueOrComponent:false ~early:true ~config
       ~requires:finalEnv.requiresEarly
  |> emitRequires ~importedValueOrComponent:finalEnv.importedValueOrComponent
       ~early:false ~config ~requires:finalEnv.requires
  |> Emitters.toString ~separator:"\n\n"
