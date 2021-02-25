
@preprocessor typescript

# Lexer
@{%

/* eslint-disable */
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

const nodeData = (children: ASTNode[]) => ({
  nodeType: "Substance",
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
      const restNodes = rest.map((ts: any[]) => ts[3]);
      return concat(first, ...restNodes);
    } else return first;
  }
%}

# Main grammar

input -> statements {% 
  ([d]): SubProg => {
    const statements = flatten(d) as SubStmt[];
    return { ...nodeData(statements), ...rangeFrom(statements), tag: "SubProg", statements };
  }
%}

statements
    # base case
    -> _ {% () => [] %} 
    # whitespaces at the beginning (NOTE: comments are allowed)
    |  _c_ nl statements {% nth(2) %} # 
    # spaces around each statement (NOTE: still wrap in list to spread later)
    # NOTE: parse comments at the end to avoid no newline at EOF
    |  _ statement _c_ {% d => [d[1]] %}
    # whitespaces in between and at the end (NOTE: comments are allowed)
    |  _ statement _c_ nl statements {% d => [d[1], ...d[4]] %}

statement 
  -> decl            {% id %}
  |  bind            {% id %}
  |  decl_bind       {% id %} 
  |  apply_predicate {% id %}
  |  label_stmt      {% id %}
  |  equal_exprs     {% id %}
  |  equal_predicates {% id %}

decl -> type_constructor __ sepBy1[identifier, ","] {%
  ([type, , ids]): Decl[] => ids.map((name: Identifier): Decl => ({
    ...nodeData([type, name]),
    ...rangeBetween(type, name),
    tag: "Decl", type, name
  }))
%}

bind -> identifier _ ":=" _ sub_expr {%
  ([variable, , , , expr]): Bind => ({
    ...nodeData([variable, expr]),
    ...rangeBetween(variable, expr),
    tag: "Bind", variable, expr
  })
%}

decl_bind -> type_constructor __ identifier _ ":=" _ sub_expr {%
  ([type, , variable, , , , expr]): [Decl, Bind] => {
    const decl: Decl = {
      ...nodeData([type, variable]),
      ...rangeBetween(type, variable),
      tag: "Decl", type, name: variable
    };
    const bind: Bind = {
      ...nodeData([variable, expr]),
      ...rangeBetween(variable, expr),
      tag: "Bind", variable, expr
    };
    return [decl, bind];
  }
%}

apply_predicate -> identifier _ "(" _ sepBy1[pred_arg, ","] _ ")" {%
  ([name, , , , args]): ApplyPredicate => ({
    ...nodeData([name, ...args]),
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
    ...nodeData([variable, field]),
    ...rangeBetween(variable, field),
    tag: "Deconstructor", variable, field
  })
%}

# NOTE: generic func type for consturction, predicate, or function
func -> identifier _ "(" _ sepBy[sub_expr, ","] _ ")" {%
  ([name, , , , args]): Func => ({
    ...nodeData([name, ...args]),
    ...rangeFrom([name, ...args]),
    tag: "Func", name, args
  })
%}

equal_exprs -> sub_expr _ "=" _ sub_expr {%
  ([left, , , , right]): EqualExprs => ({
    ...nodeData([left, right]),
    ...rangeBetween(left, right),
    tag: "EqualExprs", left, right
  })
%}

equal_predicates -> apply_predicate _ "<->" _ apply_predicate {%
  ([left, , , , right]): EqualPredicates => ({
    ...nodeData([left, right]),
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
    ...nodeData([variable, label]),
    ...rangeBetween(rangeOf(kw), label),
    tag: "LabelDecl", variable, label
  })
%}

no_label -> "NoLabel" __ sepBy1[identifier, ","] {%
  ([kw, , args]): NoLabel => ({
    ...nodeData([...args]),
    ...rangeFrom([rangeOf(kw), ...args]),
    tag: "NoLabel", args
  })
%}

auto_label -> "AutoLabel" __ label_option {%
  ([kw, , option]): AutoLabel => ({
    ...nodeData([option]),
    ...rangeBetween(kw, option),
    tag: "AutoLabel", option 
  })
%}

label_option 
  -> "All" {% ([kw]): LabelOption => ({ ...nodeData([]), ...rangeOf(kw), tag: "DefaultLabels" }) %}
  |  sepBy1[identifier, ","] {% 
       ([variables]): LabelOption => ({ ...nodeData([]), ...rangeFrom(variables), tag: "LabelIDs", variables }) 
     %}

# Grammar from Domain

type_constructor -> identifier type_arg_list:? {% 
  ([name, a]): TypeConsApp => {
    const args = optional(a, []);
    return {
      ...nodeData([name, ...args]),
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
    ...nodeData([]),
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.value
  })
%}

tex_literal -> %tex_literal {% 
  ([d]): IStringLit => ({
    ...nodeData([]),
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.text.substring(1, d.text.length - 1), // NOTE: remove dollars
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