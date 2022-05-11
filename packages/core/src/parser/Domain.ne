
@preprocessor typescript

# Lexer
@{%
/* eslint-disable */
import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'
import { optional, tokensIn, basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from 'parser/ParserUtil'
import { C, ConcreteNode, IStringLit } from "types/ast";
import { DomainProg, TypeDecl, PredicateDecl, FunctionDecl, ConstructorDecl, PreludeDecl, NotationDecl, SubTypeDecl, TypeConstructor, Type, Arg, Prop } from "types/domain";

// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  ...basicSymbols,
  identifier: {
    match: /[A-z_][A-Za-z_0-9]*/,
    type: moo.keywords({
      // NOTE: the next line add type annotation keywords into the keyword set and thereby forbidding users to use keywords like `shape`
      // "type-keyword": styleTypes, 
      type: "type",
      value: "value",
      constructor: "constructor",
      function: "function",
      predicate: "predicate",
      notation: "notation",
      prop: "Prop"
    })
  }
});

const nodeData = { nodeType: "Domain" as const };

%} # end of lexer

@lexer lexer

# Macros

@include "macros.ne"

# Main grammar

input -> statements {% 
  ([statements]): DomainProg<C> => ({
    ...nodeData,
    ...rangeFrom(statements),
    tag: "DomainProg",
    statements
  })
%}

statements
    # base case
    -> _ {% () => [] %} 
    # whitespaces at the beginning (NOTE: comments are allowed)
    |  _c_ nl statements {% nth(2) %} # 
    # spaces around each statement (NOTE: still wrap in list to spread later)
    |  _ statement _c_ {% d => [d[1]] %}
    # whitespaces in between and at the end (NOTE: comments are allowed)
    |  _ statement _c_ nl statements {% d => [d[1], ...d[4]] %}

statement 
  -> type_decl   {% id %}
  |  predicate   {% id %}
  |  function    {% id %}
  |  constructor_decl {% id %}
  |  prelude     {% id %}
  |  notation    {% id %}
  |  subtype     {% id %}

# not to be confused with `type`, defined below
type_decl -> "type" __ identifier (_ "(" _ type_params _ ")"):? (_ "<:" _ sepBy1[type, ","]):? {%
  ([typ, , name, ps, sub]): TypeDecl<C> => {
    const params = ps ? ps[3] : [];
    const superTypes = sub ? sub[3] : [];
    return { 
      ...nodeData,
      ...rangeBetween(typ, name),
      tag: "TypeDecl", name, params, superTypes
    };
  }
%}

predicate -> "predicate" __ identifier type_params_list args_list {%
  ([kw, , name, params, args]): PredicateDecl<C> => ({
    ...nodeData,
    ...rangeFrom([rangeOf(kw), ...args, ...params]),
    tag: "PredicateDecl", name, params, args
  })
%}

function 
  -> "function" __ identifier type_params_list args_list _ "->" _ arg
  {%
    ([kw, , name, ps, as, , , , output]): FunctionDecl<C> => {
      const params = optional(ps, []);
      const args   = optional(as, []);
      return {
        ...nodeData,
        ...rangeBetween(rangeOf(kw), output),
        tag: "FunctionDecl", name, output, params, args
      };
    }
  %}

# NOTE: nearley does not like `constructor` as a rule name
constructor_decl
  -> "constructor" __  identifier type_params_list named_args_list _ "->" _ arg
  {%
    ([kw, , name, ps, as, , , , output]): ConstructorDecl<C> => {
      const params = optional(ps, []);
      const args   = optional(as, []);
      return {
        ...nodeData,
        ...rangeBetween(rangeOf(kw), output),
        tag: "ConstructorDecl", name, output, params, args
      }
    }
  %}

prelude -> "value" __ var _ ":" _ type {%
  ([kw, , name, , , , type]): PreludeDecl<C> => ({
    ...nodeData,
    ...rangeBetween(rangeOf(kw), type),
    tag: "PreludeDecl", name, type
  })
%}

notation -> "notation" _  string_lit  _ "~" _ string_lit {%
  ([kw, , from, , , , to]): NotationDecl<C> => ({
    ...nodeData,
    ...rangeBetween(rangeOf(kw), to),
    tag: "NotationDecl", from, to
  })
%} 

subtype -> type _ "<:" _ type {%
  ([subType, , , , superType]): SubTypeDecl<C> => ({
    ...nodeData,
    ...rangeBetween(subType, superType),
    tag: "SubTypeDecl", subType, superType
  })
%}

# predicate 
#   -> nested_predicate {% id %}
#   |  simple_predicate {% id %}
# nested_predicate ->  "predicate" __ identifier _ ":" _ prop_list:? {%
#   ([kw, , name, , , , args]): NestedPredicateDecl => ({
#     // HACK: keywords don't seem to have ranges. Have to manually convert here
#     ...rangeFrom(tokensIn([rangeOf(kw), args])),
#     tag: "NestedPredicateDecl",
#     name, args
#   })
# %}

# Basic types
  
var -> identifier {% id %}

# TODO: without `'`, type_var will look the same as 0-arg type_constructor
type_var -> "'" identifier {% 
  ([a, name]) => ({ 
    ...nodeData,
    ...rangeBetween(a, name), 
    tag: "TypeVar", name 
  }) 
%}

type
  -> type_var         {% id %}
  |  type_constructor {% id %}
  |  prop             {% id %}

type_constructor -> identifier type_arg_list:? {% 
  ([name, a]): TypeConstructor<C> => {
    const args = optional(a, []);
    return {
      ...nodeData,
      ...rangeFrom([name, ...args]),
      tag: "TypeConstructor", name, args 
    };
  }
 %}

# Various kinds of parameters and arguments

type_arg_list -> _ "(" _ sepBy1[type, ","] _ ")" {% ([, , , d]): Type<C>[] => flatten(d) %}

type_params_list 
  -> null {% d => [] %}
  |  _ "[" _ type_params _ "]" {% nth(3) %}
type_params -> sepBy1[type_var, ","] {% ([d]) => d %}

args_list 
  -> _ "(" _ sepBy[arg, ","] _ ")" {% ([, , , d]): Arg<C>[] => flatten(d) %}
arg -> type (__ var):? {% 
  ([type, v]): Arg<C> => {
    const variable = v ? v[1] : undefined;
    const range = variable ? rangeBetween(variable, type) : rangeOf(type);
    return { 
      ...nodeData,
      ...range, 
      tag: "Arg", variable, type 
    };
  }
%}
named_args_list 
  -> _ "(" _ sepBy[named_arg, ","] _ ")" {% ([, , , d]): Arg<C>[] => flatten(d) %}
named_arg -> type __ var {% 
  ([type, , variable]): Arg<C> => ({
     ...nodeData,
     ...rangeBetween(type, variable), 
     tag: "Arg", variable, type 
  })
%}

prop -> "Prop" {% 
  ([kw]): Prop<C> => ({
     ...nodeData,
     ...rangeOf(kw), 
     tag: "Prop" 
  })  
%}

# Common 

string_lit -> %string_literal {%
  ([d]): IStringLit<C> => ({
    ...nodeData,
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.value
  })
%}

identifier -> %identifier {% 
  ([d]) => ({
    ...nodeData,
    ...rangeOf(d),
    tag: 'Identifier',
    value: d.text,
    type: "identifier"
  })
%}

# white space definitions 
comment 
  -> %comment {% convertTokenId %}
  |  %multiline_comment {% ([d]) => rangeOf(d) %}

_c_ -> (%ws | comment):* 

_ml -> multi_line_ws_char:* 

multi_line_ws_char
    -> %ws
    |  %nl
    | comment # skip comments

nl -> %nl

__ -> %ws:+ 

_ -> %ws:*
