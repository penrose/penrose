
@preprocessor typescript

# Lexer
@{%

import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'
import { basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from 'parser/ParserUtil'

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

# Grammar from Domain

# Main grammar


input -> _
# input -> statements 

# statements
#     # base case
#     -> _ {% () => [] %} 
#     # whitespaces at the beginning (NOTE: comments are allowed)
#     |  _c_ "\n" statements {% nth(2) %} # 
#     # spaces around each statement (NOTE: still wrap in list to spread later)
#     |  _ statement _ {% d => [d[1]] %}
#     # whitespaces in between and at the end (NOTE: comments are allowed)
#     |  _ statement _c_ "\n" statements {% d => [d[1], ...d[4]] %}

# statement 
#   -> type        {% id %}


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