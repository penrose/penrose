
@preprocessor typescript

# Lexer
@{%

/* eslint-disable */
import moo from "moo";
import _ from 'lodash'
import { optional, basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from './ParserUtil.js'
import { C, ConcreteNode, Identifier, StringLit } from "../types/ast.js";
import { IndexSet, RangeAssign, Range, NumberConstant, BinaryExpr, UnaryExpr, ComparisonExpr, BooleanExpr, BinaryBooleanExpr, UnaryBooleanExpr, BooleanConstant, SubProg, SubStmt, Decl, DeclList, Bind, DeclBind, ApplyPredicate, Func, TypeApp, LabelDecl, NoLabel, AutoLabel, LabelOption, LiteralSubExpr } from "../types/substance.js";


// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  tex_literal: /\$.*?\$/, // TeX string enclosed by dollar signs
  double_arrow: "<->",
  float_literal: /([+-]?([0-9]*[.])?[0-9]+)/,
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
      let: "Let",
      bool_true: "true",
      bool_false: "false",
      for: "for",
      in: "in",
      where: "where",
      mod: "mod",
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
  -> stmt_iset {% id %}
  |  stmt     {% id %}

stmt_iset -> stmt __ iset {% 
  ([stmt, , iset]) => {
    return {
      ...nodeData,
      ...rangeFrom([stmt, iset]),
      tag: "StmtSet",
      stmt, iset
    }
  }
%}


iset
  -> "for" __ sepBy1[range_assign, ","] __ "where" __ boolean_expr {% 
      ([kw, , d, ,,,b]): IndexSet<C> => {
        return {
          ...nodeData,
          ...rangeBetween(kw, b),
          tag: "IndexSet", 
          indices: d, condition: b
        };
      }
  %}
  |  "for" __ sepBy1[range_assign, ","] {% 
      ([kw, , d]): IndexSet<C> => {
        return {
          ...nodeData,
          ...rangeBetween(kw, d[d.length - 1]),
          tag: "IndexSet", 
          indices: d, condition: undefined
        };
      }
     %}

range_assign -> identifier _ "in" _ int_range {%
  ([variable, , , , range]): RangeAssign<C> => ({
    ...nodeData,
    ...rangeBetween(variable, range),
    tag: "RangeAssign", variable, range
  })
%}

int_range -> "[" _ number _ "," _ number _ "]" {%
  ([lbracket, , low, , , , high, , rbracket]): Range<C> => ({
    ...nodeData,
    ...rangeBetween(lbracket, rbracket),
    tag: "Range", low, high
  })
%}

float -> %float_literal {% 
    ([d]): NumberConstant<C> => ({
      ...nodeData,
      ...rangeOf(d),
      tag: "NumberConstant", contents: +d.value
    })
  %}

number -> float {%id%}

stmt 
  -> decl            {% id %}
  |  bind            {% id %}
  |  let_bind        {% id %}   
  |  decl_bind       {% id %} 
  |  apply_predicate {% id %}
  |  label_stmt      {% id %}

decl -> type_app __ sepEndBy1[identifier, ","] {%
  ([type, , ids]): Decl<C> | DeclList<C> => {
    if (ids.length === 1) {
      // single identifier means one decl
      return {
        ...nodeData,
        ...rangeFrom([type, ...ids]),
        tag: "Decl",
        type, name: ids[0]
      };
    } else {
      return {
        ...nodeData,
        ...rangeFrom([type, ...ids]),
        tag: "DeclList",
        type, names: ids
      }
    }
  }
%}

bind -> identifier _ ":=" _ sub_expr {%
  ([variable, , , , expr]): Bind<C> => ({
    ...nodeData,
    ...rangeBetween(variable, expr),
    tag: "Bind", variable, expr
  })
%}

decl_bind -> type_app __ identifier _ ":=" _ sub_expr {%
  ([type, , variable, , , , expr]): DeclBind<C> => {
    return {
      ...nodeData,
      ...rangeBetween(type, expr),
      tag: "DeclBind",
      type, variable, expr
    };
  }
%}

let_bind -> "Let" __ identifier _ ":=" _ sub_expr {%
  ([prefix, , variable, , , , expr]): DeclBind<C> => {
    const type: TypeApp<C> = {
      ...nodeData,
      ...rangeBetween(variable, expr),
      tag: "TypeApp", name: expr.name
    };
    return {
      ...nodeData,
      ...rangeBetween(type, expr),
      tag: "DeclBind",
      type, variable, expr
    };
  }
%}

apply_predicate -> identifier _ "(" _ sepEndBy[sub_arg_expr, ","] _ ")" {%
  ([name, , , , args]): ApplyPredicate<C> => ({
    ...nodeData,
    ...rangeFrom([name, ...args]),
    tag: "ApplyPredicate", name, args
  })
%}

pred_arg -> sub_expr {% id %} # allow any expressions as arguments

sub_expr 
  -> func {% id %}
  |  sub_arg_expr {% id %}

sub_arg_expr
  -> identifier {% id %}
  |  literal_sub_expr {% id %}

literal_sub_expr
  -> string_lit {% 
      ([sl]): LiteralSubExpr<C> => ({
        ...nodeData,
        ...rangeFrom([sl]),
        tag: "LiteralSubExpr",
        contents: sl
      }) 
    %}
   | number {%
      ([num]): LiteralSubExpr<C> => ({
        ...nodeData,
        ...rangeFrom([num]),
        tag: "LiteralSubExpr",
        contents: num
      }) 
   %}

# NOTE: generic func type for consturction, predicate, or function
func -> identifier _ "(" _ sepEndBy[sub_arg_expr, ","] _ ")" {%
  ([name, , , , args]): Func<C> => ({
    ...nodeData,
    ...rangeFrom([name, ...args]),
    tag: "Func", name, args
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

type_app -> identifier {% 
  ([name]): TypeApp<C> => {
    return {
      ...nodeData,
      ...rangeFrom([name]),
      tag: "TypeApp", name 
    };
  }
%}


# Exprs

expr -> 
    expr _ "+" _ term {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "+", left, right}) %}
  | expr _ "-" _ term {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "-", left, right}) %}
  | term {% id %}

term -> 
    term _ "^" _ factor {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "^", left, right}) %}
  | term _ "*" _ factor {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "*", left, right}) %}
  | term _ "/" _ factor {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "/", left, right}) %}
  | term _ "%" _ factor {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "%", left, right}) %}
  | term _ "mod" _ factor {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "%", left, right}) %}
  | "-" _ factor {% ([op, , arg]): UnaryExpr<C> => ({...nodeData, ...rangeBetween(op, arg), tag: "UnaryExpr", operator: "-", arg }) %}
  | factor {% id %}

factor -> 
    "(" _ expr _ ")" {% nth(2) %}
  | number {% id %}
  | identifier {% id %}

comparison_expr -> 
    expr _ "<" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: "<", left, right}) %}
  | expr _ ">" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: ">", left, right}) %}
  | expr _ "<=" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: "<=", left, right}) %}
  | expr _ ">=" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: ">=", left, right}) %}
  | expr _ "==" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: "==", left, right}) %}
  | expr _ "!=" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: "!=", left, right}) %}

boolean_expr ->
    boolean_expr _ "||" _ boolean_term {% ([left, , , , right]): BinaryBooleanExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryBooleanExpr", operator: "||", left, right}) %}
  | boolean_term {% id %}

boolean_term ->
    boolean_term _ "&&" _ boolean_factor {% ([left, , , , right]): BinaryBooleanExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryBooleanExpr", operator: "&&", left, right}) %}
  | "!" _ boolean_factor {% ([op, , arg]): UnaryBooleanExpr<C> => ({...nodeData, ...rangeBetween(op, arg), tag: "UnaryBooleanExpr", operator: "!", arg}) %}
  | boolean_factor {% id %}

boolean_factor ->
    "(" _ boolean_expr _ ")" {% nth(2) %}
  | "true" {% ([kw]): BooleanConstant<C> => ({...nodeData, ...rangeOf(kw), tag: "BooleanConstant", value: true}) %}
  | "false" {% ([kw]): BooleanConstant<C> => ({...nodeData, ...rangeOf(kw), tag: "BooleanConstant", value: false}) %}
  | comparison_expr {% id %}
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
