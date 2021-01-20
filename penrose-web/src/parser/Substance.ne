
@preprocessor typescript

# Lexer
@{%

import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'
import { optional, basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from 'parser/ParserUtil'

// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  tex_literal: /\$.*?\$/,
  double_arrow: "<->",
  ...basicSymbols,
  // tex_literal: /\$(?:[^\n\$]|\\["\\ntbfr])*\$/,
  identifier: {
    match: /[A-z_][A-Za-z_0-9]*/,
    type: moo.keywords({
      // NOTE: the next line add type annotation keywords into the keyword set and thereby forbidding users to use keywords like `shape`
      // "type-keyword": styleTypes, 
      all: "All",
      label: "Label",
      noLabel: "NoLabel",
      autoLabel: "AutoLabel"
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
      const restNodes = rest.map((ts: any[]) => ts[3]);
      return concat(first, ...restNodes);
    } else return first;
  }
%}

# Main grammar

input -> statements {% 
  ([d]): SubProg => {
    const statements = flatten(d) as SubStmt[];
    return { ...rangeFrom(statements), tag: "SubProg", statements };
  }
%}

statements
    # base case
    -> _ {% () => [] %} 
    # whitespaces at the beginning (NOTE: comments are allowed)
    |  _c_ "\n" statements {% nth(2) %} # 
    # spaces around each statement (NOTE: still wrap in list to spread later)
    # NOTE: parse comments at the end to avoid no newline at EOF
    |  _ statement _c_ {% d => [d[1]] %}
    # whitespaces in between and at the end (NOTE: comments are allowed)
    |  _ statement _c_ "\n" statements {% d => [d[1], ...d[4]] %}

statement 
  -> decl            {% id %}
  |  bind            {% id %}
  # |  decl_bind   {% id %} # TODO: shorthand for bind
  |  apply_predicate {% id %}
  |  label_stmt      {% id %}
  |  equal_exprs     {% id %}
  |  equal_predicates {% id %}

decl -> type_constructor __ sepBy1[identifier, ","] {%
  ([type, , ids]): Decl[] => ids.map((name: Identifier): Decl => ({
    ...rangeBetween(type, name),
    tag: "Decl", type, name
  }))
%}

bind -> identifier _ ":=" _ sub_expr {%
  ([variable, , , , expr]): Bind => ({
    ...rangeBetween(variable, expr),
    tag: "Bind", variable, expr
  })
%}

apply_predicate -> identifier _ "(" _ sepBy1[pred_arg, ","] _ ")" {%
  ([name, , , , args]): ApplyPredicate => ({
    ...rangeFrom([name, ...args]),
    tag: "ApplyPredicate", name, args
  })
%}

pred_arg -> sub_expr {% id %} # allow any expressions as arguments

sub_expr 
  -> identifier {% id %}
  |  deconstructor {% id %}
  |  func {% id %}
  |  string_lit {% id %}

deconstructor -> identifier _ "." _ identifier {%
  ([variable, , , , field]): Deconstructor => ({
    ...rangeBetween(variable, field),
    tag: "Deconstructor", variable, field
  })
%}

# NOTE: generic func type for consturction, predicate, or function
func -> identifier _ "(" _ sepBy[sub_expr, ","] _ ")" {%
  ([name, , , , args]): Func => ({
    ...rangeFrom([name, ...args]),
    tag: "Func", name, args
  })
%}

equal_exprs -> sub_expr _ "=" _ sub_expr {%
  ([left, , , , right]): EqualExprs => ({
    ...rangeBetween(left, right),
    tag: "EqualExprs", left, right
  })
%}

equal_predicates -> func _ "<->" _ func {%
  ([left, , , , right]): EqualPredicates => ({
    ...rangeBetween(left, right),
    tag: "EqualPredicates", left, right
  })
%}

label_stmt 
  -> label_decl {% id %}
  |  no_label   {% id %}
  |  auto_label {% id %}

label_decl -> "Label" __ identifier __ tex_literal {%
  ([kw, , variable, , label]): LabelDecl => ({
    ...rangeBetween(rangeOf(kw), label),
    tag: "LabelDecl", variable, label
  })
%}

no_label -> "NoLabel" __ sepBy1[identifier, ","] {%
  ([kw, , args]): NoLabel => ({
    ...rangeFrom([rangeOf(kw), ...args]),
    tag: "NoLabel", args
  })
%}

auto_label -> "AutoLabel" __ label_option {%
  ([kw, , option]): AutoLabel => ({
    ...rangeBetween(kw, option),
    tag: "AutoLabel", option 
  })
%}

label_option 
  -> "All" {% ([kw]): LabelOption => ({ ...rangeOf(kw), tag: "DefaultLabels" }) %}
  |  sepBy1[identifier, ","] {% 
       ([variables]): LabelOption => ({ ...rangeFrom(variables), tag: "LabelIDs", variables }) 
     %}

# Grammar from Domain

type_constructor -> identifier type_arg_list:? {% 
  ([name, a]): TypeConsApp => {
    const args = optional(a, []);
    return {
      ...rangeFrom([name, ...args]),
      tag: "TypeConstructor", name, args 
    };
  }
%}

# NOTE: only type constructors are alloed in Substance
type_arg_list -> _ "(" _ sepBy1[type_constructor, ","] _ ")" {% 
  ([, , , d]): TypeConsApp[] => flatten(d) 
%}

# Common 

string_lit -> %string_literal {%
  ([d]): IStringLit => ({
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.text
  })
%}

tex_literal -> %tex_literal {% 
  ([d]): IStringLit => ({
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.text.substring(1, d.text.length - 1), // NOTE: remove dollars
  })
%}

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