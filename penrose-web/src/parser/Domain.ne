
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
  lte: "<=",
  lt: "<",
  gte: ">=",
  gt: ">",
  eq: "==",
  rarrow: "->",
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

# TODO: factor out sepEndBy
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
  # |  constructor {% id %}
  # |  prelude     {% id %}
  # |  notation    {% id %}
  # |  subtype     {% id %}

# TODO: parse the y with kind case as well, or is it obsolete?
type -> "type" __ identifier (_ "(" _ type_params _ ")"):? {%
  ([typ, , name, params]): TypeDecl => ({
    ...rangeBetween(typ, name),
    tag: "TypeDecl", 
    name, 
    params: params ? params[3] : []
  })
%}
predicate 
  -> nested_predicate {% id %}
  |  simple_predicate {% id %}

# TODO: check if dangling colon is okay.
simple_predicate -> "predicate" __ identifier _ ":" type_params_list:? _ args_list:? {%
  ([kw, , name, , , params, , args]): PredicateDecl => ({
    // HACK: keywords don't seem to have ranges. Have to manually convert here
    ...rangeFrom(tokensIn([rangeOf(kw), args, params])),
    tag: "PredicateDecl",
    name, 
    params: optional(params, []),
    args: optional(args, []),
  })
%}

# TODO: check if dangling colon is okay.
nested_predicate ->  "predicate" __ identifier _ ":" _ prop_list:? {%
  ([kw, , name, , , , args]): NestedPredicateDecl => ({
    // HACK: keywords don't seem to have ranges. Have to manually convert here
    ...rangeFrom(tokensIn([rangeOf(kw), args])),
    tag: "NestedPredicateDecl",
    name, args
  })
%}

function 
  -> "function" __ identifier _ ":" type_params_list:? _ named_args_list:? _ "->" _ arg
  {%
    ([kw, , name, , , params, , args, , , , output]): FunctionDecl => ({
      ...rangeBetween(rangeOf(kw), output),
      tag: "FunctionDecl",
      name, output,
      params: optional(params, []),
      args: optional(args, []),
    })
  %}

# Basic types
  
variable 
  -> var      {% id %}
  |  type_var {% id %}

var -> identifier {% ([name]): VarConst => ({ ...rangeOf(name), tag: "VarConst", name }) %}

type_var -> "'" identifier {% 
  ([a, name]) => ({ ...rangeBetween(a, name), tag: "TypeVar", name }) 
%}

kind 
  -> type   {% id %}
  |  "type" {% ([d]): ConstType => ({ ...rangeOf(d), tag: "ConstType", contents: "type" }) %}

type
  -> type_var         {% id %}
  |  type_constructor {% id %}

type_constructor -> identifier type_arg_list:? {% 
  ([name, args]): TypeConstructor => ({
    ...rangeBetween(name, args),
    tag: "TypeConstructor",
    name, 
    args: optional(args, [])
  })
 %}

# Various kinds of parameters and arguments

type_params_list -> _ "[" _ type_params _ "]" {% nth(3) %}
type_params -> sepBy1[type_param, ","] {% ([d]) => d %}
type_param -> variable _ ":" _ kind {% 
  ([variable, , , , kind]): TypeParam => ({
    ...rangeBetween(variable, kind),
    tag: "TypeParam", variable, kind
  })
%}

type_arg_list -> _ "(" _ sepBy1[type_arg, ","] _ ")" {% ([, , , d]): TypeArg[] => flatten(d) %}
type_arg 
  -> var  {% id %}
  |  type {% id %}

# args_list -> _ "(" _ args _ ")"        {% nth(3) %}
args_list -> sepBy1[arg, "*"] {% ([d]): Arg[] => flatten(d) %}
named_args_list -> sepBy1[named_arg, "*"] {% ([d]): Arg[] => flatten(d) %}
arg -> type (__ var):? {% 
  ([type, v]): Arg => {
    const variable = v ? v[1] : undefined;
    const range = variable ? rangeBetween(variable, type) : rangeOf(type);
    return { ...range, tag: "Arg", variable: variable, type };
  }
%}
named_arg -> type __ var {% 
  ([type, , variable]): Arg => ({
     ...rangeBetween(type, variable), 
     tag: "Arg", variable: variable, type 
  })
%}

prop -> "Prop" _ var {% nth(2) %}
prop_list -> sepBy1[prop, "*"] {% ([d]) => d %}

# TODO: finish below
# function -> "function" __ identifier
# constructor -> "constructor" __ identifier
# prelude -> "prelude" __ identifier
# notation -> "notation" __ identifier
# subtype -> "subtype" __ identifier

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
