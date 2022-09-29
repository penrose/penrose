@preprocessor typescript

# Lexer
@{%

/* eslint-disable */
import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'
import { basicSymbols, rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from 'parser/ParserUtil'
import { C, ConcreteNode, Identifier, StringLit  } from "types/ast";
import { StyT, DeclPattern, DeclPatterns, RelationPatterns, Namespace, Selector, StyProg, HeaderBlock, RelBind, RelField, RelPred, SEFuncOrValCons, SEBind, Block, AnonAssign, Delete, Override, PathAssign, StyType, BindingForm, Path, Layering, BinaryOp, Expr, BinOp, SubVar, StyVar, UOp, List, Tuple, Vector, BoolLit, Vary, Fix, CompApp, ObjFn, ConstrFn, GPIDecl, PropertyDecl, ColorLit
} from "types/style";

const styleTypes: string[] =
  [ "scalar"
  , "int"
  , "bool"
  , "string"
  , "path"
  , "color"
  , "file"
  , "style"
  , "shape" // TODO: make sure this is the intended keyword
  , "vec2"
  , "vec3"
  , "vec4"
  , "mat2x2"
  , "mat3x3"
  , "mat4x4"
  , "function"
  , "objective"
  , "constraint"
  ];

// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  ...basicSymbols,
  identifier: {
    // Allow latin/greek upper/lower characters, numbers, underscores.  An identified name:
    //  - May not begin with a number (ending with a number is ok)
    //  - May be a single latin/greek upper/lower character or underscore
    //  - May begin or end with an underscore
    match: /[A-Za-z_\u0374-\u03FF][A-Za-z_0-9\u0374-\u03FF]*/,
    type: moo.keywords({
      // NOTE: the next line add type annotation keywords into the keyword set and thereby forbidding users to use keywords like `shape`
      // "type-keyword": styleTypes, 
      forall: "forall",
      where: "where",
      with: "with",
      delete: "delete",
      as: "as",
      true: "true",
      false: "false",
      layer: "layer",
      encourage: "encourage",
      ensure: "ensure",
      override: "override",
    })
  }
});

const nodeData = { nodeType: "Style" as const };

// Node constructors

const declList = (type: StyT<C>, ids: BindingForm<C>[]): DeclPattern<C>[] => {
  const decl = (t: StyT<C>, i: BindingForm<C>): DeclPattern<C> => ({
    ...nodeData,
    ...rangeFrom([t, i]),
    tag: "DeclPattern",
    type: t,
    id: i 
  });
  return ids.map((i: BindingForm<C>) => decl(type, i));
}


const selector = (
  hd: DeclPatterns<C>,
  wth?: DeclPatterns<C>,
  whr?: RelationPatterns<C>,
  namespace?: Namespace<C>
): Selector<C> => {
  return {
    ...nodeData,
    ...rangeFrom(compact([hd, wth, whr])),
    tag: "Selector",
    head: hd,
    with: wth,
    where: whr,
  };
}

const layering = (kw: any, left: Path<C>, layeringOp: "above" | "below", right: Path<C>[]): Layering<C> => ({
    ...nodeData,
    ...rangeFrom(kw ? [rangeOf(kw), left, ...right] : [left, ...right]),
    tag: 'Layering', left, right, layeringOp
})


const binop = (op: BinaryOp, left: Expr<C>, right: Expr<C>): BinOp<C> => ({
  ...nodeData,
  ...rangeBetween(left, right),
  tag: 'BinOp', op, left, right
})

%} # end of lexer

@lexer lexer

# Macros

@include "macros.ne"

################################################################################
# Style Grammar
# NOTE: remember that parens in the grammar or EBNF operators (e.g. `:*`) will wrap an array around the result. 

# TODO: header_blocks gets called twice here. Investigate why
input -> _ml header_blocks {%
  ([, blocks]): StyProg<C> => ({
    ...nodeData,
    ...rangeFrom(blocks),
    tag: "StyProg", blocks
  })
%}

header_blocks -> header_block:* {% id %}

header_block -> header block _ml {%
  ([header, block]): HeaderBlock<C> => ({
    ...nodeData,
    ...rangeFrom([header, block]), 
    tag:"HeaderBlock", header, block 
  })
%}

################################################################################
# Selector grammar

header 
  -> selector  {% id %}
  |  namespace {% id %}

selector -> 
    forall decl_patterns _ml  
    {% (d) => selector(d[1], undefined, undefined) %}
  | forall decl_patterns _ml select_where 
    {% (d) => selector(d[1], undefined, d[3]) %} 
  | forall decl_patterns _ml select_with 
    {% (d) => selector(d[1], d[3], undefined)%}   
  | forall decl_patterns _ml select_where select_with 
    {% (d) => selector(d[1], d[4], d[3]) %} 
  | forall decl_patterns _ml select_with select_where 
    {% (d) => selector(d[1], d[3], d[4]) %} 

forall -> "forall" __ {% nth(0) %}

select_with -> "with" __ decl_patterns _ml {% d => d[2] %}

decl_patterns -> sepBy1[decl_list, ";"] {% 
  ([d]): DeclPatterns<C> => {
    const contents = flatten(d) as DeclPattern<C>[];
    return {
      ...nodeData,
      ...rangeFrom(contents),
      tag: "DeclPatterns", contents
    };
  }
%}

decl_list -> identifier __ sepBy1[binding_form, ","] {% 
  ([type, , ids]): DeclPattern<C>[] => {
    return declList(type, ids);
  }
%}

select_where -> "where" __ relation_list _ml {% d => d[2] %}

relation_list -> sepBy1[relation, ";"]  {% 
  ([d]): RelationPatterns<C> => ({
    ...nodeData,
    ...rangeFrom(d),
    tag: "RelationPatterns",
    contents: d
  })
%}

relation
  -> rel_bind {% id %} 
  |  rel_pred {% id %}
  |  rel_field {% id %}

rel_bind 
  -> binding_form _ ":=" _ sel_expr {% 
    ([id, , , , expr]): RelBind<C> => ({
      ...nodeData,
      ...rangeFrom([id, expr]),
      tag: "RelBind", id, expr,
    })
  %}

rel_pred 
  -> identifier _ "(" pred_arg_list ")" __ml alias_as {% // aliasing case
    ([name, , , args, , , a]): RelPred<C> => ({
        ...nodeData,
        ...rangeFrom([name, ...args, a]),
        tag: "RelPred", name, args, alias: a
    })
  %} 
  | identifier _ "(" pred_arg_list ")" {% 
    ([name, , , args, ,]): RelPred<C> => ({
        ...nodeData,
        ...rangeFrom([name, ...args]),
        tag: "RelPred", name, args,
    })
  %} 

alias_as -> "as" __ identifier {% d => d[2] %} 

rel_field -> binding_form __ "has" __ (field_desc __):? identifier {%
  ([name, , , , field_desc, field]): RelField<C> => ({
    ...nodeData,
    ...rangeBetween(name, field),
    tag: "RelField",
    fieldDescriptor: field_desc ? field_desc[0] : undefined,
    name, field, 
  })
%}

field_desc 
  -> "math" {% () => "MathLabel" %} 
  |  "text" {% () => "TextLabel" %}

sel_expr_list 
  -> _ {% d => [] %}
  |  _ sepBy1[sel_expr, ","] _ {% nth(1) %}

sel_expr 
  -> identifier _ "(" sel_expr_list ")" {% 
    ([name, , , args, ]): SEFuncOrValCons<C> => ({
      ...nodeData,
      ...rangeFrom([name, ...args]),
      tag: "SEFuncOrValCons",
      name, args
    }) 
  %}
  |  binding_form {% ([d]): SEBind<C> => ({
      ...nodeData,
      ...rangeFrom([d]), 
      tag: "SEBind", contents: d
    }) 
  %}

pred_arg_list 
  -> _ {% d => [] %}
  |  _ sepBy1[pred_arg, ","] _ {% nth(1) %}

# NOTE: resolve ambiguity here by allowing only rel_pred or `binding_form`
# Can't use sel_expr because sel_expr has valcons or func, which looks exactly the same as predicates. 
pred_arg 
  -> rel_pred {% id %}
  |  binding_form {% ([d]): SEBind<C> => ({
      ...nodeData,
      ...rangeFrom([d]), 
      tag: "SEBind", contents: d
    }) 
  %}

binding_form 
  -> subVar {% id %} 
  |  styVar {% id %}

# HACK: tokens like "`" don't really have start and end points, just line and col. How do you merge a heterogenrous list of tokens and nodes?
subVar -> "`" identifier "`" {%
  ([ltick, contents, rtick]): SubVar<C> => ({ 
    ...nodeData,
    ...rangeBetween(rangeOf(ltick), rangeOf(rtick)),
    tag: "SubVar", contents
  })
%}

styVar -> identifier {%
  ([contents]): StyVar<C> => ({ 
    ...nodeData,
    ...rangeOf(contents), 
    tag: "StyVar", contents
  })
%}


namespace -> styVar _ml {%
  ([contents]): Namespace<C> => ({
    ...nodeData,
    ...rangeOf(contents),
    tag: "Namespace",
    contents
  })
%}

################################################################################
# Block grammar

block -> "{" statements "}" {% 
  ([lbrace, stmts, rbrace]): Block<C> => ({
    ...nodeData,
    ...rangeBetween(lbrace, rbrace),
    tag: "Block",
    statements: stmts
  })
%}

statements
    # base case
    -> _ {% () => [] %} 
    # whitespaces at the beginning (NOTE: comments are allowed)
    |  _c_ nl statements {% nth(2) %} # 
    # spaces around each statement (NOTE: still wrap in list to spread later)
    |  _ statement _ {% d => [d[1]] %}
    # whitespaces in between and at the end (NOTE: comments are allowed)
    |  _ statement _c_ nl statements {% d => [d[1], ...d[4]] %}

statement 
  -> delete {% id %}
  |  override {% id %}
  |  path_assign {% id %}
  |  anonymous_expr {% ([contents]): AnonAssign<C> => 
    ({
      ...nodeData,
      ...rangeOf(contents), 
      tag: "AnonAssign", contents
    }) 
  %}

delete -> "delete" __ path {%
  ([kw, , contents]): Delete<C> => {
   return {
    ...nodeData,
    ...rangeBetween(rangeOf(kw), contents),
    tag: "Delete", contents
  }}
%}

override -> "override" __ path _ "=" _ assign_expr {%
  ([kw, , path, , , , value]): Override<C> => ({ 
    ...nodeData,
    ...rangeBetween(kw, value),
    tag: "Override", path, value
  })
%}

path_assign -> type:? __ path _ "=" _ assign_expr {%
  ([type, , path, , , , value]): PathAssign<C> => ({ 
    ...nodeData,
    ...rangeBetween(type ? type : path, value),
    tag: "PathAssign",
    type, path, value
  })
%}

type 
  -> identifier {% ([contents]): StyType<C> => ({
      ...nodeData,
      ...rangeOf(contents),
      tag: 'TypeOf', contents
    }) 
  %}
  |  identifier "[]" {% ([contents]): StyType<C> => ({
      ...nodeData,
      ...rangeOf(contents), 
      tag: 'ListOf', contents
    }) 
  %}

path -> binding_form ("." identifier):* (_ access_ops):? {%
  ([name, dotParts, accesses]) => {
    const members = dotParts.map((d: [unknown, Expr<C>]) => d[1]);
    const indices = accesses === null ? [] : accesses[1];
    return {
      ...nodeData,
      ...rangeFrom([name, ...members, ...indices]),
      tag: "Path", name, members, indices
    };
  }
%}

# TODO: capture the range more accurately, now it's missing the last bracket
access_ops 
  -> access_op  
  |  access_op _ access_ops {% (d: any[]) => [d[0], ...d[2]] %}
access_op -> "[" _ expr _ "]" {% nth(2) %}

# Expression

# NOTE: all assign_expr can appear on the rhs of an assign statement, but not all can be, say, arguments of a computation/constraint. 
assign_expr 
  -> expr {% id %}
  |  layering {% id %}
  |  objective {% id %}
  |  constraint {% id %}
  |  gpi_decl {% id %}

anonymous_expr
  -> layering {% id %}
  |  objective {% id %}
  |  constraint {% id %}
  |  gpi_decl {% id %}

# NOTE: inline computations on expr_literal (including expr_literal)
expr -> arithmeticExpr {% id %}

# Arith ops (NOTE: ordered in decreasing precedence)

# Parenthsized
parenthesized 
  -> "(" _ arithmeticExpr _ ")" {% nth(2) %}
  |  expr_literal               {% id %}

# Unary ops 
unary 
  -> "-" _ parenthesized  {% (d): UOp<C> => 
    ({
      ...nodeData,
      ...rangeBetween(d[0], d[2]),
      tag: 'UOp', op: "UMinus", arg: d[2]
    }) 
  %}
  |  parenthesized        {% id %}

# Exponents
factor 
  -> unary _ "^" _ factor {% (d): BinOp<C> => binop('Exp', d[0], d[4]) %}
  |  unary                {% id %}

# Multiplication and division
term 
  -> term _ "*" _ factor  {% (d): BinOp<C> => binop('Multiply', d[0], d[4]) %}
  |  term _ "/" _ factor  {% (d): BinOp<C> => binop('Divide', d[0], d[4]) %}
  |  factor               {% id %}

# Addition and subtraction
arithmeticExpr 
  -> arithmeticExpr _ "+" _ term {% (d): BinOp<C> => binop('BPlus', d[0], d[4]) %}
  |  arithmeticExpr _ "-" _ term {% (d): BinOp<C> => binop('BMinus', d[0], d[4]) %}
  |  term                        {% id %}

# NOTE: all of the expr_literal can be operands of inline computation 
expr_literal
  -> bool_lit {% id %}
  |  color_lit {% id %}
  |  string_lit {% id %}
  |  annotated_float {% id %}
  |  computation_function {% id %}
  |  path {% id %}
  |  list {% id %}
  |  tuple {% id %}
  |  vector {% id %}
  # TODO: 
  # |  transformExpr 

list -> "[" _ expr_list _ "]" {% 
  ([lbracket, , exprs, , rbracket]): List<C> => ({
    ...nodeData,
    ...rangeBetween(lbracket, rbracket),
    tag: 'List',
    contents: exprs
  })
%}

# NOTE: we only allow 2 tuples and enforce this rule during parsing
tuple -> "{" _ expr _ "," _ expr _ "}" {% 
  ([lbrace, , e1, , , , e2, , rbrace]): Tuple<C> => ({
    ...nodeData,
    ...rangeBetween(lbrace, rbrace),
    tag: 'Tuple',
    contents: [e1, e2]
  })
%}

# NOTE: the extra expr makes sure a vector will always have >1 components.
vector -> "(" _ expr  _ "," expr_list ")" {% 
  ([lparen, , first, , , rest, rparen]): Vector<C> => ({
    ...nodeData,
    ...rangeBetween(lparen, rparen),
    tag: 'Vector',
    contents: [first, ...rest]
  })
%}

bool_lit -> ("true" | "false") {%
  ([[d]]): BoolLit<C> => ({
    ...nodeData,
    ...rangeOf(d),
    tag: 'BoolLit',
    contents: d.text === 'true' // https://stackoverflow.com/questions/263965/how-can-i-convert-a-string-to-boolean-in-javascript
  })
%}

color_lit 
  -> %hex_literal
  {% ([d]): ColorLit<C> => ({
    ...nodeData,
    ...rangeOf(d), 
    tag: "ColorLit",
    contents: d.text.slice(1, d.text.length)
  })
 %}

string_lit -> %string_literal {%
  ([d]): StringLit<C> => ({
    ...nodeData,
    ...rangeOf(d),
    tag: 'StringLit',
    contents: d.value
  })
%}

annotated_float 
  -> "?" {% ([d]): Vary<C> => ({ ...nodeData, ...rangeOf(d), tag: 'Vary' }) %}
  |  %float_literal {% 
    ([d]): Fix<C> => ({ ...nodeData, ...rangeOf(d), tag: 'Fix', contents: parseFloat(d) }) 
  %}

layering
  -> layer_keyword:? path __ layer_op __ path_list {% (d): Layering<C> => layering(d[0], d[1], d[3], d[5]) %}

layer_keyword -> "layer" __ {% nth(0) %}

layer_op 
  -> "below" {% () => "below" %} 
  |  "above" {% () => "above" %}

path_list -> sepBy1[expr, ","] {% id %}

computation_function -> identifier _ "(" expr_list ")" {% 
  ([name, , , args, rparen]): CompApp<C> => ({
    ...nodeData,
    ...rangeBetween(name, rparen),
    tag: "CompApp",
    name, args
  }) 
%}

objective -> "encourage" __ identifier _ "(" expr_list ")" {% 
  ([kw, , name, , , args, rparen]): ObjFn<C> => ({
    ...nodeData,
    ...rangeBetween(kw, rparen),
    tag: "ObjFn",
    name, args
  }) 
%}

constraint -> "ensure" __ identifier _ "(" expr_list ")" {% 
  ([kw, , name, , , args, rparen]): ConstrFn<C> => ({
    ...nodeData,
    ...rangeBetween(kw, rparen),
    tag: "ConstrFn",
    name, args
  }) 
%}

expr_list 
  -> _ {% d => [] %}
  |  _ sepBy1[expr, ","] _ {% nth(1) %}

gpi_decl -> identifier _ml "{" property_decl_list "}" {%
  ([shapeName, , , properties, rbrace]): GPIDecl<C> => ({
    ...nodeData,
    ...rangeBetween(shapeName, rbrace),
    tag: "GPIDecl",
    shapeName, properties
  })
%}

# TODO: maybe separate out as macro when stable (macros have really bad state ids tho!)
property_decl_list
    # base case
    -> _ {% () => [] %} 
    # whitespaces at the beginning (NOTE: comments are allowed)
    |  _c_ nl property_decl_list {% nth(2) %} # 
    # spaces around each decl (NOTE: still wrap in list to spread later)
    |  _ property_decl _ {% d => [d[1]] %}
    # whitespaces in between and at the end (NOTE: comments are allowed)
    |  _ property_decl _c_ nl property_decl_list {% d => [d[1], ...d[4]] %}
  
property_decl -> identifier _ ":" _ expr {%
  ([name, , , , value]): PropertyDecl<C> => ({
    ...nodeData,
    ...rangeBetween(name, value),
    tag: "PropertyDecl",
    name, value
  })
%}

# Common 

identifier -> %identifier {% 
  ([d]): Identifier<C> => ({
    ...nodeData,
    ...rangeOf(d),
    tag: 'Identifier',
    value: d.text,
    type: styleTypes.includes(d.text) ? "type-keyword" : "identifier"
  })
%}

# white space definitions 

comment 
  -> %comment {% convertTokenId %}
  |  %multiline_comment {% ([d]) => rangeOf(d) %}

_c_ -> (%ws | comment):* 

_ml -> multi_line_ws_char:* 

__ml -> multi_line_ws_char:+

multi_line_ws_char
    -> %ws
    |  %nl
    | comment # skip comments

nl -> %nl

__ -> %ws:+ 

_ -> %ws:*
