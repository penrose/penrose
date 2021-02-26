
@preprocessor typescript

# Lexer
@{%
/* eslint-disable */
import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'
import { optional, tokensIn, basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from 'parser/ParserUtil'

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

const nodeData = (children: ASTNode[]) => ({
  nodeType: "Domain",
  children
});

%} # end of lexer

@lexer lexer

# Macros

sepBy1[ITEM, SEP] -> $ITEM (_ $SEP _ $ITEM):* $SEP:? {% 
  d => { 
    const [first, rest] = [d[0], d[1]];
    if(rest.length > 0) {
      const restNodes = rest.map((ts: any[]) => ts[3]);
      return concat(first, ...restNodes);
    } else return first;
  }
%}

sepBy[ITEM, SEP] -> $ITEM:? (_ $SEP _ $ITEM):* {% 
  d => { 
    const [first, rest] = [d[0], d[1]];
    if(!first) return [];
    if(rest.length > 0) {
      const restNodes = rest.map(ts => ts[3]);
      return concat(first, ...restNodes);
    } else return first;
  }
%}

# Main grammar

input -> statements {% 
  ([statements]): DomainProg => ({
    ...nodeData(statements),
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
  -> type        {% id %}
  |  predicate   {% id %}
  |  function    {% id %}
  |  constructor_decl {% id %}
  |  prelude     {% id %}
  |  notation    {% id %}
  |  subtype     {% id %}

type -> "type" __ identifier (_ "(" _ type_params _ ")"):? {%
  ([typ, , name, ps]): TypeDecl => {
    const params = ps ? ps[3] : [];
    return { 
      ...nodeData([name, ...params]),
      ...rangeBetween(typ, name),
      tag: "TypeDecl", name, params
    };
  }
%}

predicate -> "predicate" __ identifier type_params_list args_list {%
  ([kw, , name, params, args]): PredicateDecl => ({
    ...nodeData([name, ...params, ...args]),
    ...rangeFrom([rangeOf(kw), ...args, ...params]),
    tag: "PredicateDecl", name, params, args
  })
%}

function 
  -> "function" __ identifier type_params_list args_list _ "->" _ arg
  {%
    ([kw, , name, ps, as, , , , output]): FunctionDecl => {
      const params = optional(ps, []);
      const args   = optional(as, []);
      return {
        ...nodeData([name, output, ...params, ...args]),
        ...rangeBetween(rangeOf(kw), output),
        tag: "FunctionDecl", name, output, params, args
      };
    }
  %}

# NOTE: nearley does not like `constructor` as a rule name
constructor_decl
  -> "constructor" __  identifier type_params_list named_args_list _ "->" _ arg
  {%
    ([kw, , name, ps, as, , , , output]): ConstructorDecl => {
      const params = optional(ps, []);
      const args   = optional(as, []);
      return {
        ...nodeData([name, output, ...params, ...args]),
        ...rangeBetween(rangeOf(kw), output),
        tag: "ConstructorDecl", name, output, params, args
      }
    }
  %}

prelude -> "value" __ var _ ":" _ type {%
  ([kw, , name, , , , type]): PreludeDecl => ({
    ...nodeData([name, type]),
    ...rangeBetween(rangeOf(kw), type),
    tag: "PreludeDecl", name, type
  })
%}

notation -> "notation" _  string_lit  _ "~" _ string_lit {%
  ([kw, , from, , , , to]): NotationDecl => ({
    ...nodeData([from, to]),
    ...rangeBetween(rangeOf(kw), to),
    tag: "NotationDecl", from, to
  })
%} 

subtype -> type _ "<:" _ type {%
  ([subType, , , , superType]): SubTypeDecl => ({
    ...nodeData([subType, superType]),
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
    ...nodeData([name]),
    ...rangeBetween(a, name), 
    tag: "TypeVar", name 
  }) 
%}

type
  -> type_var         {% id %}
  |  type_constructor {% id %}
  |  prop             {% id %}

type_constructor -> identifier type_arg_list:? {% 
  ([name, a]): TypeConstructor => {
    const args = optional(a, []);
    return {
      ...nodeData([name, ...args]),
      ...rangeFrom([name, ...args]),
      tag: "TypeConstructor", name, args 
    };
  }
 %}

# Various kinds of parameters and arguments

type_arg_list -> _ "(" _ sepBy1[type, ","] _ ")" {% ([, , , d]): Type[] => flatten(d) %}

type_params_list 
  -> null {% d => [] %}
  |  _ "[" _ type_params _ "]" {% nth(3) %}
type_params -> sepBy1[type_var, ","] {% ([d]) => d %}

args_list 
  -> null {% d => [] %}
  |  _ ":" _ sepBy1[arg, "*"] {% ([, , , d]): Arg[] => flatten(d) %}
arg -> type (__ var):? {% 
  ([type, v]): Arg => {
    const variable = v ? v[1] : undefined;
    const range = variable ? rangeBetween(variable, type) : rangeOf(type);
    return { 
      ...nodeData(variable ? [variable, type] : [type]),
      ...range, 
      tag: "Arg", variable, type 
    };
  }
%}
named_args_list 
  -> null {% d => [] %}
  |  _ ":" _ sepBy1[named_arg, "*"] {% ([, , , d]): Arg[] => flatten(d) %}
named_arg -> type __ var {% 
  ([type, , variable]): Arg => ({
     ...nodeData([type, variable]),
     ...rangeBetween(type, variable), 
     tag: "Arg", variable, type 
  })
%}

prop -> "Prop" {% 
  ([kw]): Prop => ({
     ...nodeData([]),
     ...rangeOf(kw), 
     tag: "Prop" 
  })  
%}

# Common 

string_lit -> %string_literal {%
  ([d]): IStringLit => ({
    ...nodeData([]),
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.value
  })
%}

identifier -> %identifier {% 
  ([d]) => ({
    ...nodeData([]),
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
