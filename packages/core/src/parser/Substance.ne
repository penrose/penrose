
@preprocessor typescript

# Lexer
@{%

/* eslint-disable */
import moo from "moo";
import _ from 'lodash'
import { optional, basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from './ParserUtil.js'
import { C, ConcreteNode, Identifier, StringLit } from "../types/ast.js";
import { Sequence, RangeAssign, IntRange, BinaryExpr, ComparisonExpr, IndexedIdentifier, SubProg, SubStmt, Decl, Bind, ApplyPredicate, Deconstructor, Func, EqualExprs, EqualPredicates, LabelDecl, NoLabel, AutoLabel, LabelOption, TypeConsApp, IntLit, Index } from "../types/substance.js";


// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  tex_literal: /\$.*?\$/, // TeX string enclosed by dollar signs
  double_arrow: "<->",
  int_literal: /(?:0|[1-9][0-9]*)/,              
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
      bool_false: "false"
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
  -> stmt_seq {% id %}
  |  stmt     {% id %}

# TODO: `stmt` should include indexed_variable
stmt_seq -> stmt __ "for" __ sequence {% 
  ([[stmt], , , , seq]) => {
    return {
      ...nodeData,
      ...rangeFrom([stmt, seq]),
      tag: "StmtSeq",
      stmt, seq
    }
  }
%}

# TODO: add comparsion expression
sequence -> sepBy1[range_assign, ","] {% 
  ([d]): Sequence<C> => ({
    ...nodeData,
    ...rangeFrom(d),
    tag: "Sequence", 
    indices: d, conditions: []
  })
%}

range_assign -> identifier _ "=" _ int_range {%
  ([variable, , , , range]): RangeAssign<C> => ({
    ...nodeData,
    ...rangeBetween(variable, range),
    tag: "RangeAssign", variable, range
  })
%}

int_range -> "[" _ int_lit _ "," _ int_lit _ "]" {%
  ([lbracket, , low, , , , high, , rbracket]): IntRange<C> => ({
    ...nodeData,
    ...rangeBetween(lbracket, rbracket),
    tag: "IntRange", low, high
  })
%}

int_lit -> %int_literal {% 
  ([d]): IntLit<C> => ({
    ...nodeData,
    ...rangeOf(d),
    tag: "IntLit", value: +d.value
  })
%}

# TODO: `stmt` should include indexed_variable 
indexed_identifier -> identifier "{" _ index _ "}" {%
  ([name, , , index , , rbrace]): IndexedIdentifier<C> => ({
    ...nodeData,
    ...rangeBetween(name, rbrace),
    tag: "IndexedIdentifier", name, index
  }) 
%}

index 
  -> int_lit {% 
    ([d]): Index<C> => ({
      ...nodeData,
      ...rangeOf(d),
      tag: "Index", content: d
    })
  %}
  |  identifier {% 
    ([d]): Index<C> => ({
      ...nodeData,
      ...rangeOf(d),
      tag: "Index", content: d
    })
  %}

stmt 
  -> decl            {% id %}
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

# Exprs

# TODO: `mod` operator and other arithmetic ops
expr -> 
    expr _ "+" _ term {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "+", left, right}) %}
  | expr _ "-" _ term {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "-", left, right}) %}
  | term {% id %}

term -> 
    term _ "*" _ factor {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "*", left, right}) %}
  | term _ "/" _ factor {% ([left, , , , right]): BinaryExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "BinaryExpr", operator: "/", left, right}) %}
  | factor {% id %}

factor -> 
    "(" _ expr _ ")" {% nth(2) %}
  | int_lit {% id %}
  | identifier {% id %}

comparison_expr -> 
    expr _ "<" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: "<", left, right}) %}
  | expr _ ">" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: ">", left, right}) %}
  | expr _ "<=" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: "<=", left, right}) %}
  | expr _ ">=" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: ">=", left, right}) %}
  | expr _ "==" _ expr {% ([left, , , , right]): ComparisonExpr<C> => ({...nodeData, ...rangeBetween(left, right), tag: "ComparisonExpr", operator: "==", left, right}) %}

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
