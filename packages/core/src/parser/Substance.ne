
@preprocessor typescript

# Lexer
@{%

/* eslint-disable */
import moo from "moo";
import _ from 'lodash'
import { optional, basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from './ParserUtil.js'
import { C, ConcreteNode, Identifier, StringLit } from "../types/ast.js";
import { IndexedIdentifier, SubProg, SubStmt, Decl, Bind, ApplyPredicate, Deconstructor, Func, EqualExprs, EqualPredicates, LabelDecl, NoLabel, AutoLabel, LabelOption, TypeConsApp } from "../types/substance.js";


// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  tex_literal: /\$.*?\$/, // TeX string enclosed by dollar signs
  double_arrow: "<->",
  ellipsis: "...",
  ...basicSymbols,
  identifier: {
    match: /[A-z_][A-Za-z_0-9]*/,
    type: moo.keywords({
      // NOTE: the next line add type annotation keywords into the keyword set and thereby forbidding users to use keywords like `shape`
      // "type-keyword": styleTypes, 
      all: "All",
      label: "Label",
      noLabel: "NoLabel",
      autoLabel: "AutoLabel",
      let: "Let"
    })
  }
});

const nodeData = { nodeType: "Substance" as const };

%} # end of lexer

@lexer lexer

# Macros

@include "macros.ne"

# Main grammar

input -> statements {% 
  ([d]): SubProg<C> => {
    const statements = _.flatten(d) as SubStmt<C>[];
    return { ...nodeData, ...rangeFrom(statements), tag: "SubProg", statements };
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
  -> decl_seq        {% id %}
  |  decl            {% id %}
  |  bind            {% id %}
  |  let_bind        {% id %}   
  |  decl_bind       {% id %} 
  |  apply_predicate {% id %}
  |  label_stmt      {% id %}
  |  equal_exprs     {% id %}
  |  equal_predicates {% id %}

decl -> type_constructor __ sepEndBy1[identifier, ","] {%
  ([type, , ids]): Decl<C>[] => ids.map((name: Identifier<C>): Decl<C> => ({
    ...nodeData,
    ...rangeBetween(type, name),
    tag: "Decl", type, name
  }))
%}

ellipsis_sep -> "," _ "..." _ "," {% id %}

indexed_identifier -> identifier "{" _ %float_literal _ "}" {%
  ([name, , , index]): IndexedIdentifier<C> => ({
    ...nodeData,
    ...rangeBetween(name, index),
    tag: "IndexedIdentifier", name, index: +index.value
  }) 
%}

decl_seq -> type_constructor __ sepBy1[indexed_identifier, ","] _ ellipsis_sep _ indexed_identifier {%
  ([type, , leading, , , , last]): DeclSeq<C> => {
    return {
      ...nodeData,
      ...rangeBetween(type, last),
      tag: "DeclSeq", type, leading, last
    };
  }
%}

bind -> identifier _ ":=" _ sub_expr {%
  ([variable, , , , expr]): Bind<C> => ({
    ...nodeData,
    ...rangeBetween(variable, expr),
    tag: "Bind", variable, expr
  })
%}

decl_bind -> type_constructor __ identifier _ ":=" _ sub_expr {%
  ([type, , variable, , , , expr]): [Decl<C>, Bind<C>] => {
    const decl: Decl<C> = {
      ...nodeData,
      ...rangeBetween(type, variable),
      tag: "Decl", type, name: variable
    };
    const bind: Bind<C> = {
      ...nodeData,
      ...rangeBetween(variable, expr),
      tag: "Bind", variable, expr
    };
    return [decl, bind];
  }
%}

let_bind -> "Let" __ identifier _ ":=" _ sub_expr {%
  ([prefix, , variable, , , , expr]): [Decl<C>, Bind<C>] => {
    const type: TypeConsApp<C> = {
        ...nodeData,
        ...rangeBetween(variable, expr),
        tag: "TypeConstructor", args: [], name: expr.name
      };
    const decl: Decl<C> = {
      ...nodeData,
      ...rangeBetween(type, variable),
      tag: "Decl", type, name: variable
    };
    const bind: Bind<C> = {
      ...nodeData,
      ...rangeBetween(variable, expr),
      tag: "Bind", variable, expr
    };
    return [decl, bind];
  }
%}

apply_predicate -> identifier _ "(" _ sepEndBy1[pred_arg, ","] _ ")" {%
  ([name, , , , args]): ApplyPredicate<C> => ({
    ...nodeData,
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
  ([variable, , , , field]): Deconstructor<C> => ({
    ...nodeData,
    ...rangeBetween(variable, field),
    tag: "Deconstructor", variable, field
  })
%}

# NOTE: generic func type for consturction, predicate, or function
func -> identifier _ "(" _ sepEndBy[sub_expr, ","] _ ")" {%
  ([name, , , , args]): Func<C> => ({
    ...nodeData,
    ...rangeFrom([name, ...args]),
    tag: "Func", name, args
  })
%}

equal_exprs -> sub_expr _ "=" _ sub_expr {%
  ([left, , , , right]): EqualExprs<C> => ({
    ...nodeData,
    ...rangeBetween(left, right),
    tag: "EqualExprs", left, right
  })
%}

equal_predicates -> apply_predicate _ "<->" _ apply_predicate {%
  ([left, , , , right]): EqualPredicates<C> => ({
    ...nodeData,
    ...rangeBetween(left, right),
    tag: "EqualPredicates", left, right
  })
%}

label_stmt 
  -> label_decl {% id %}
  |  no_label   {% id %}
  |  auto_label {% id %}

label_decl 
  -> "Label" __ identifier __ tex_literal {%
    ([kw, , variable, , label]): LabelDecl<C> => ({
      ...nodeData,
      ...rangeBetween(rangeOf(kw), label),
      tag: "LabelDecl", 
      labelType: "MathLabel",
      variable, label
    })
  %}
 |  "Label" __ identifier __ string_lit {%
    ([kw, , variable, , label]): LabelDecl<C> => ({
      ...nodeData,
      ...rangeBetween(rangeOf(kw), label),
      tag: "LabelDecl", 
      labelType: "TextLabel",
      variable, label
    })
  %}

no_label -> "NoLabel" __ sepEndBy1[identifier, ","] {%
  ([kw, , args]): NoLabel<C> => ({
    ...nodeData,
    ...rangeFrom([rangeOf(kw), ...args]),
    tag: "NoLabel", args
  })
%}

auto_label -> "AutoLabel" __ label_option {%
  ([kw, , option]): AutoLabel<C> => ({
    ...nodeData,
    ...rangeBetween(kw, option),
    tag: "AutoLabel", option 
  })
%}

label_option 
  -> "All" {% ([kw]): LabelOption<C> => ({ ...nodeData, ...rangeOf(kw), tag: "DefaultLabels" }) %}
  |  sepEndBy1[identifier, ","] {% 
       ([variables]): LabelOption<C> => ({ ...nodeData, ...rangeFrom(variables), tag: "LabelIDs", variables }) 
     %}

# Grammar from Domain

type_constructor -> identifier type_arg_list:? {% 
  ([name, a]): TypeConsApp<C> => {
    const args = optional(a, []);
    return {
      ...nodeData,
      ...rangeFrom([name, ...args]),
      tag: "TypeConstructor", name, args 
    };
  }
%}

# NOTE: only type constructors are alloed in Substance
type_arg_list -> _ "(" _ sepEndBy1[type_constructor, ","] _ ")" {% 
  ([, , , d]): TypeConsApp<C>[] => _.flatten(d) 
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

tex_literal -> %tex_literal {% 
  ([d]): StringLit<C> => ({
    ...nodeData,
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.text.substring(1, d.text.length - 1), // NOTE: remove dollars
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
