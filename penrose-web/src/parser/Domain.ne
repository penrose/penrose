
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
  lparen: "(",
  rparen: ")",
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
      notation: "notation"
    })
  }
});

%} # end of lexer

@lexer lexer

input -> statements 

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
  |  constructor {% id %}
  |  prelude     {% id %}
  |  notation    {% id %}
  |  subtype     {% id %}

# TODO: parse the y with kind case as well, or is it obsolete?
type -> "type" __ identifier 

# TODO: finish below
predicate -> "predicate" __ identifier
function -> "function" __ identifier
constructor -> "constructor" __ identifier
prelude -> "prelude" __ identifier
notation -> "notation" __ identifier
subtype -> "subtype" __ identifier

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
