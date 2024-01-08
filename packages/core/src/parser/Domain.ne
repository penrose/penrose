
@preprocessor typescript

# Lexer
@{%
/* eslint-disable */
import moo from "moo";
import _ from 'lodash'
import { optional, tokensIn, basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from './ParserUtil.js'
import { C, ConcreteNode, StringLit } from "../types/ast.js";
import { DomainProg, TypeDecl, FunctionDecl, ConstructorDecl, SubTypeDecl, PredicateDecl, Type, Arg } from "../types/domain.js";

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
      symmetric: "symmetric",
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
  |  subtype     {% id %}

# not to be confused with `type`, defined below
type_decl -> "type" __ identifier (_ "<:" _ sepEndBy1[type, ","]):? {%
  ([typ, , name, sub]): TypeDecl<C> => {
    const superTypes = sub ? sub[3] : [];
    return { 
      ...nodeData,
      ...rangeBetween(typ, name),
      tag: "TypeDecl", name, superTypes
    };
  }
%}

# This now works with symmetric predicate declarations.
predicate -> ("symmetric" __):? "predicate" __ identifier args_list {%
  ([sym, kw, , name, args]): PredicateDecl<C> => {
    var isSymmetric = sym !== null;
    return {
      ...nodeData,
      ...rangeFrom([
        // If "symmetric" exists, include it.
        // sym[0] is the token "symmetric";
        // the rest is white space
        ...(isSymmetric ? [rangeOf(sym[0])] : []),
        rangeOf(kw), 
        ...args
      ]),
      tag: "PredicateDecl", name, args,
      // If "symmetric" isn't present, "sym" would be null and this would be false
      // Otherwise, this would be true.
      symmetric: isSymmetric
    }
  }
%}

function 
  -> "function" __ identifier args_list _ "->" _ arg
  {%
    ([kw, , name, as, , , , output]): FunctionDecl<C> => {
      const args   = optional(as, []);
      return {
        ...nodeData,
        ...rangeBetween(rangeOf(kw), output),
        tag: "FunctionDecl", name, output, args
      };
    }
  %}

# NOTE: nearley does not like `constructor` as a rule name
constructor_decl
  -> long_constructor_decl {% id %}
  |  short_constructor_decl {% id %}
  

# Constructor decl supplying output type
long_constructor_decl 
  -> "constructor" __  identifier named_args_list _ "->" _ arg
  {%
    ([kw, , name, as, , , , output]): ConstructorDecl<C> => {
      const args   = optional(as, []);
      return {
        ...nodeData,
        ...rangeBetween(rangeOf(kw), output),
        tag: "ConstructorDecl", name, output, args
      }
    }
  %}

# Constructor decl such that constructor's name is read as output type
short_constructor_decl 
  -> "constructor" __  identifier named_args_list
  {%
    ([kw, , name, as]): ConstructorDecl<C> => {
      const args   = optional(as, []);
      const type: Type<C> = {
        ...nodeData,
        ...rangeBetween(rangeOf(kw), name),
        tag: "Type", name: name
      };
      const output: Arg<C> = {
        ...nodeData, 
        ...rangeBetween(rangeOf(kw), name),
        tag: "Arg", type, variable: undefined
      };
      return {
        ...nodeData,
        ...rangeFrom([rangeOf(kw), ...args]),
        tag: "ConstructorDecl", name, output, args
      }
    }
  %}

subtype -> type _ "<:" _ type {%
  ([subType, , , , superType]): SubTypeDecl<C> => ({
    ...nodeData,
    ...rangeBetween(subType, superType),
    tag: "SubTypeDecl", subType, superType
  })
%}

# Basic types
  
var -> identifier {% id %}

type -> identifier {% 
  ([name]): Type<C> => {
    return {
      ...nodeData,
      ...rangeFrom([name]),
      tag: "Type", name 
    };
  }
 %}

# Various kinds of parameters and arguments


args_list 
  -> _ "(" _ sepEndBy[arg, ","] _ ")" {% ([, , , d]): Arg<C>[] => _.flatten(d) %}
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
  -> _ "(" _ sepEndBy[named_arg, ","] _ ")" {% ([, , , d]): Arg<C>[] => _.flatten(d) %}
named_arg -> type __ var {% 
  ([type, , variable]): Arg<C> => ({
     ...nodeData,
     ...rangeBetween(type, variable), 
     tag: "Arg", variable, type 
  })
%}

# Common 

string_lit -> %string_literal {%
  ([d]): StringLit<C> => ({
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
