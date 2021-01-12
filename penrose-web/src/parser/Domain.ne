
@preprocessor typescript

# Lexer
@{%

import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'
import { rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from 'parser/ParserUtil'

// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  ws: /[ \t]+/,
  nl: { match: "\n", lineBreaks: true },
  subtypeOf: "<:",
  lte: "<=",
  lt: "<",
  gte: ">=",
  gt: ">",
  eq: "==",
  rarrow: "->",
  tilda: "~",
  lparen: "(",
  rparen: ")",
  apos: "'",
  comma: ",",
  string_literal: /"(?:[^\n\\"]|\\["\\ntbfr])*"/,
  float_literal: /[+-]?(?:\d+(?:[.]\d*)?(?:[eE][+-]?\d+)?|[.]\d+(?:[eE][+-]?\d+)?)/,
  comment: /--.*?$/,
  multiline_comment: { 
    match: /\/\*(?:[\s\S]*?)\*\//,
    lineBreaks: true 
  },
  dot: ".",
  brackets: "[]",
  lbracket: "[",
  rbracket: "]",
  lbrace: "{",
  rbrace: "}",
  assignment: "=",
  def: ":=",
  plus: "+",
  exp: "^",
  minus: "-",
  multiply: "*",
  divide: "/",
  modulo: "%",
  colon: ":",
  semi: ";",
  question: "?",
  dollar: "$",
  tick: "\`",
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

const optional = <T>(optionalValue: T | undefined, defaultValue: T) => optionalValue ? optionalValue : defaultValue;
// Helper that takes in a mix of single token or list of tokens, drops all undefined (i.e. optional ealues), and finally flattten the mixture to a list of tokens.
const tokensIn = (tokenList: any[]): any[] => flatten(compact(tokenList));

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

input -> statements {% 
  ([statements]): DomainProg => ({
    ...rangeFrom(statements),
    tag: "DomainProg",
    statements
  })
%}

statements
    # base case
    -> _ {% () => [] %} 
    # whitespaces at the beginning (NOTE: comments are allowed)
    |  _c_ "\n" statements {% nth(2) %} # 
    # spaces around each statement (NOTE: still wrap in list to spread later)
    |  _ statement _ {% d => [d[1]] %}
    # whitespaces in between and at the end (NOTE: comments are allowed)
    |  _ statement _c_ "\n" statements {% d => [d[1], ...d[4]] %}

statement 
  -> type        {% id %}
  |  predicate   {% id %}
  |  function    {% id %}
  |  constructor_decl {% id %}
  |  prelude     {% id %}
  |  notation    {% id %}
  |  subtype     {% id %}

type -> "type" __ identifier (_ "(" _ type_params _ ")"):? {%
  ([typ, , name, params]): TypeDecl => ({
    ...rangeBetween(typ, name),
    tag: "TypeDecl", 
    name, 
    params: params ? params[3] : []
  })
%}

predicate -> "predicate" __ identifier type_params_list args_list {%
  ([kw, , name, params, args]): PredicateDecl => ({
    ...rangeFrom([rangeOf(kw), ...args, ...params]),
    tag: "PredicateDecl", name, params, args
  })
%}

function 
  -> "function" __ identifier type_params_list args_list _ "->" _ arg
  {%
    ([kw, , name, params, args, , , , output]): FunctionDecl => ({
      ...rangeBetween(rangeOf(kw), output),
      tag: "FunctionDecl",
      name, output,
      params: optional(params, []),
      args: optional(args, []),
    })
  %}

# NOTE: nearley does not like `constructor` as a rule name
constructor_decl
  -> "constructor" __  identifier type_params_list named_args_list _ "->" _ arg
  {%
    ([kw, , name, params, args, , , , output]): ConstructorDecl => ({
      ...rangeBetween(rangeOf(kw), output),
      tag: "ConstructorDecl",
      name, output,
      params: optional(params, []),
      args: optional(args, []),
    })
  %}

prelude -> "value" __ var _ ":" _ type {%
  ([kw, , name, , , , type]): PreludeDecl => ({
    ...rangeBetween(rangeOf(kw), type),
    tag: "PreludeDecl", name, type
  })
%}

notation -> "notation" _ %string_literal _ "~" _ %string_literal {%
  ([kw, , from, , , , to]): NotationDecl => ({
    ...rangeBetween(rangeOf(kw), to),
    tag: "NotationDecl", 
    from: JSON.parse(from.text),
    to: JSON.parse(to.text)
  })
%} 
subtype -> type _ "<:" _ type {%
  ([subType, , , , superType]): SubTypeDecl => ({
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
  
var -> identifier {% ([name]): VarConst => ({ ...rangeOf(name), tag: "VarConst", name }) %}

# TODO: without `'`, type_var will look the same as 0-arg type_constructor
type_var -> "'" identifier {% 
  ([a, name]) => ({ ...rangeBetween(a, name), tag: "TypeVar", name }) 
%}

type
  -> type_var         {% id %}
  |  type_constructor {% id %}
  |  prop             {% id %}

type_constructor -> identifier type_arg_list:? {% 
  ([name, a]): TypeConstructor => {
    const args = optional(a, []);
    return {
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
    return { ...range, tag: "Arg", variable, type };
  }
%}
named_args_list 
  -> null {% d => [] %}
  |  _ ":" _ sepBy1[named_arg, "*"] {% ([, , , d]): Arg[] => flatten(d) %}
named_arg -> type __ var {% 
  ([type, , variable]): Arg => ({
     ...rangeBetween(type, variable), 
     tag: "Arg", variable, type 
  })
%}

prop -> "Prop" _ var {% nth(2) %}
prop_list -> sepBy1[prop, "*"] {% ([d]) => d %}

# Common 

identifier -> %identifier {% 
  ([d]) => ({
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
    |  "\n"
    | comment # skip comments

__ -> %ws:+ 

_ -> %ws:*
