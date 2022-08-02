# ReScript Linter

An AST based linter for ReScript. Write your rule based on ReScript AST.

[![asciicast](https://asciinema.org/a/f71oYaBZ0tisjpQX7hBeyvuck.svg)](https://asciinema.org/a/f71oYaBZ0tisjpQX7hBeyvuck?autoplay=1&loop=1)

## Building the project

```
git submodule update --init --recursive
opam install . --deps-only --with-doc --with-test
dune build
```

or with Nix,

```
git submodule update --init --recursive
nix-build
```

### To run the test

```
dune runtest
```

## Running the parser

### Config file

You can set rules that you want to lint using config file. See below for list of available rules.

```json
{
  "rules": [
    {
      "rule": "DisallowOperator",
      "options": {
        "disallowed_operator": "|>",
        "suggested_operator": "->"
      }
    },
    {
      "rule": "DisallowFunction",
      "options": {
        "disallowed_function": "string_of_int",
        "suggested_function": "Belt.Int.fromString"
      }
    },
    {
      "rule": "DisallowFunction",
      "options": {
        "disallowed_function": "intOfStringOpt",
        "suggested_function": "Belt.Int.fromString"
      }
    },
    {
      "rule": "DisallowFunction",
      "options": {
        "disallowed_function": "floatOfStringOpt",
        "suggested_function": "Belt.Float.fromString"
      }
    },
    {
      "rule": "NoJStringInterpolation"
    }
  ]
}
```

Once you build the project, you can copy the resulting binary. Or you can also run it with `dune`

```
dune exec -- rescript_linter -c config.json foo.res
```

### Disabling lint

You can disable lint per file. Simply add `RSLINT_DISABLE` comment at the top of your file.

```rescript
// RSLINT_DISABLE

// this will not throw lint error
let _ = string_of_int(1)
```

## Rules

Rules are built-in in the project. Currently there's no pluggable architecture to add third party rule.

Rules are defined in `lib/rules`.

Currently, there are three rules available

1. `DisallowFunction` - Disallow the use of certain functions like `string_of_int`
2. `DisallowOperator` - Disallow the use of certain operators like `|>`
3. `NoJStringInterpolation` - Disallow the use of j-string Interpolation

### Writing your own rule

By convention, you should write a new rule on its own module in `lib/rules`.

#### Rule interface

Each rule module must have the signature of `Rule.HASRULE`.

```ocaml
module type HASRULE = sig
  type t
  val proxy : t modifier
  val meta : meta
  val lint : t -> lintResult
end
```

- `meta` allows you to define name and the rule description
- `proxy` and `type t` should be based on the type of AST that you would like to capture
- then you should write the `lint` function that receive the AST based on type `t` and this function should return either `LintOk` or `LintError`

#### Rule with options

Some rule can be designed such a way that it can be generic and user can specify options in order to create a specific rule. For example, our `DisallowedFunctionRule` is a generic rule and you can specify the function name through its option.

There is a module signature that you would have to follow to add options to a rule.

```ocaml
module type OPTIONS = sig
  type options
  val options : options
end
```

Then you would a create a module functor that accepts the options as its module argument.

```ocaml
module Options = struct
  type options = {disallowed_function: string; suggested_function: string option}
end

module Make (OPT : Rule.OPTIONS with type options = Options.options) : Rule.HASRULE with type t = Parsetree.expression = struct
  ...

  let function_name = OPT.options.Options.disallowed_function
  ...
end
```

**Using the generic rule**

Then you can use the module functor to create a specific rule based on the options that you passed.

```ocaml
module DisallowStringOfIntRule = DisallowedFunctionRule.Make (struct
  type options = DisallowedFunctionRule.Options.options

  let options =
    { DisallowedFunctionRule.Options.disallowed_function= "string_of_int"
    ; DisallowedFunctionRule.Options.suggested_function= Some "Belt.Int.fromString" }
end)
```

#### Rule configuration parser

You can add the parser that parses JSON config in `ConfigReader.ml`. That way the config will read the correct rules that you defined.

### Understanding the AST

#### Printing the AST

It is very useful to print the AST when you're investigating how to write a rule for certain code. Rescript has AST pretty printer that can come handy to convert your Rescript code to its AST.

`test.res`
```rescript
let txt = j`hello`
```

`AST`
```
$ rescript -print ast test.res
[
  structure_item (test.res[1,0+0]..[1,0+18])
    Pstr_value Nonrec
    [
      <def>
        pattern (test.res[1,0+4]..[1,0+7])
          Ppat_var "txt" (test.res[1,0+4]..[1,0+7])
        expression (test.res[1,0+11]..[1,0+17])
          attribute "res.template" (_none_[1,0+-1]..[1,0+-1]) ghost
            []
          Pexp_constant PConst_string ("hello",Some "j")
    ]
]
```

#### AST

The complete AST types can be found in [https://github.com/rescript-lang/syntax/blob/master/compiler-libs-406/parsetree.mli](https://github.com/rescript-lang/syntax/blob/master/compiler-libs-406/parsetree.mli)

The linter currently doesn't handle all cases, for now it only handles `expression` and `structure` AST.

We define GADT `modifier` type that you need to specify as the `proxy` field based on `Rule.HASRULE` signature above.

```ocaml
type _ modifier =
  | MExpression : Parsetree.expression modifier
  | MStructure : Parsetree.structure modifier
  | MPattern : Parsetree.pattern modifier
```

If you like to parse an expression, then you'd need to choose `MExpression`, same goes with `MStructure`. Which one you'd pick is mostly based on which part you'd like to lint. Playing with print the AST above will help. Most of the time `expression` can take care most of your lint requirement.

`MPattern` allows you parse variable names etc (You could potentialy use `MStructure` to do the same).
