@preprocessor typescript

# Lexer
@{%

import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'

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
  tick: "\`",
  identifier: {
    match: /[A-z_][A-Za-z_0-9]*/,
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


function tokenStart(token: any) {
  return {
      line: token.line,
      col: token.col - 1
  };
}

// TODO: test multiline range
const tokenEnd = (token: any) => {
  const { text } = token;
  const nl = /\r\n|\r|\n/;
  var newlines = 0;
  var textLength = text.length;
  if (token.lineBreaks) {
    newlines = text.split(nl).length;
    textLength = text.substring(text.lastIndexOf(nl) + 1);
  }
  return {
      line: token.line + newlines,
      col: token.col + textLength - 1
  };
}

const rangeOf = (token: any) => {
  // if it's already converted, assume it's an AST Node
  if(token.start && token.end) {
    return { start: token.start, end: token.end };
  }
  return {
    start: tokenStart(token),
    end: tokenEnd(token)
  };
}

const before = (loc1: SourceLoc, loc2: SourceLoc) => {
  if (loc1.line < loc2.line) { return true; }
  if (loc1.line > loc2.line) { return false; }
  return loc1.col < loc2.col;
}

const minLoc = (...locs: SourceLoc[]) => {
  if(locs.length === 0) throw new TypeError();
  let minLoc = locs[0];
  for(const l of locs)
    if(before(l, minLoc)) minLoc = l;
  return minLoc;
}
const maxLoc = (...locs: SourceLoc[]) => {
  if(locs.length === 0) throw new TypeError();
  let maxLoc = locs[0];
  for(const l of locs)
    if(before(maxLoc, l)) maxLoc = l;
  return maxLoc;
}

/** Given a list of tokens, find the range of tokens */
const rangeFrom = (children: ASTNode[]) => {
  // NOTE: this function is called in intermediate steps with empty lists, so will need to guard against empty lists.
  if(children.length === 0) {
    // console.trace(`No children ${JSON.stringify(children)}`);
    return { start: {line: 1, col: 1}, end: {line: 1, col: 1} };
  }

  if(children.length === 1) {
    const child = children[0];
    return { start: child.start, end: child.end };
  }

  return {
    start: minLoc(...children.map(n => n.start)),
    end: maxLoc(...children.map(n => n.end)),
    // children TODO: decide if want children/parent pointers in the tree
  }
}

const rangeBetween = (beginToken: any, endToken: any) => {
  const [beginRange, endRange] = [beginToken, endToken].map(rangeOf);
  return {
    start: beginRange.start,
    end: endRange.end
  }
}

// const walkTree = <T>(root: ASTNode, f: (ASTNode): T) => {
  // TODO: implement
// }

const convertTokenId = ([token]: any) => {
  return {
    ...rangeOf(token),
    value: token.text,
    type: token.type,
  };
}

const nth = (n: number) => {
  return function(d: any[]) {
      return d[n];
  };
}

// Node constructors

const declList = (type: StyT, ids: BindingForm[]) => {
  const decl = (t: StyT, i: BindingForm) => ({
    ...rangeFrom([t, i]),
    tag: "DeclPattern",
    type: t,
    id: i 
  });
  return ids.map((i: BindingForm) => decl(type, i));
}


const selector = (
  hd: DeclPatterns,
  wth?: DeclPatterns,
  whr?: RelationPatterns,
  namespace?: Namespace
): Selector => {
  return {
    ...rangeFrom(compact([hd, wth, whr, namespace])),
    tag: "Selector",
    head: hd,
    with: wth,
    where: whr,
    namespace,
  };
}

const layering = (kw: any, below: Path, above: Path): ILayering => ({
  // TODO: keyword in range
  ...rangeFrom([above, below]),
  tag: 'Layering',
  above, below
})

const binop = (op: BinaryOp, left: Expr, right: Expr): IBinOp => ({
  ...rangeBetween(left, right),
  tag: 'BinOp',
  op, left, right
})

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

################################################################################
# Style Grammar
# NOTE: remember that parens in the grammar or EBNF operators (e.g. `:*`) will wrap an array around the result. 

# TODO: header_blocks gets called twice here. Investigate why
input -> _ml header_blocks {%
  ([, blocks]) => ({
    ...rangeFrom(blocks),
    tag: "StyProg",
    blocks
  })
%}

header_blocks -> header_block:* {% id %}

header_block -> header block _ml {%
 ([header, block]): HeaderBlock => ({
   ...rangeFrom([header, block]), tag:"HeaderBlock", header, block })
%}

################################################################################
# Selector grammar

header 
  -> selector  {% id %}
  |  namespace {% id %}

selector -> 
    forall:? decl_patterns _ml select_as:?  
    {% (d) => selector(d[1], undefined, undefined, d[3]) %} 
  | forall:? decl_patterns _ml select_where select_as:? 
    {% (d) => selector(d[1], undefined, d[3], d[4]) %} 
  | forall:? decl_patterns _ml select_with select_as:? 
    {% (d) => selector(d[1], d[3], undefined, d[4]) %} 
  | forall:? decl_patterns _ml select_where select_with select_as:? 
    {% (d) => selector(d[1], d[4], d[3], d[5]) %} 
  | forall:? decl_patterns _ml select_with select_where select_as:? 
    {% (d) => selector(d[1], d[3], d[4], d[5]) %}

forall -> "forall" __ {% nth(0) %}

select_with -> "with" __ decl_patterns _ml {% d => d[2] %}

decl_patterns -> sepBy1[decl_list, ";"] {% 
  ([d]) => {
    const contents = flatten(d) as ASTNode[];
    return {
      ...rangeFrom(contents),
      tag: "DeclPatterns", contents
    };
  }
%}

decl_list -> identifier __ sepBy1[binding_form, ","] {% 
  ([type, , ids]) => {
    return declList(type, ids);
  }
%}

select_where -> "where" __ relation_list _ml {% d => d[2] %}

relation_list -> sepBy1[relation, ";"]  {% ([d]) => ({
    ...rangeFrom(d),
    tag: "RelationPatterns",
    contents: d
  })
%}

relation
  -> rel_bind {% id %} 
  |  rel_pred {% id %}

rel_bind -> binding_form _ ":=" _ sel_expr {%
  ([id, , , , expr]) => ({
    ...rangeFrom([id, expr]),
    tag: "RelBind",
    id, expr
  })
%}

rel_pred -> identifier _ "(" pred_arg_list ")" {% 
  ([name, , , args, ]): RelPred => ({
    ...rangeFrom([name, ...args]),
    tag: "RelPred",
    name, args
  }) 
%}

sel_expr_list 
  -> _ {% d => [] %}
  |  _ sepBy1[sel_expr, ","] _ {% nth(1) %}

sel_expr 
  -> identifier _ "(" sel_expr_list ")" {% ([name, , , args, ]) => ({
      ...rangeFrom([name, ...args]),
      tag: "SEFuncOrValCons",
      name, args
    }) 
  %}
  |  binding_form {% ([d]) => ({...rangeFrom([d]), tag: "SEBind", contents: d}) %}


pred_arg_list 
  -> _ {% d => [] %}
  |  _ sepBy1[pred_arg, ","] _ {% nth(1) %}

# NOTE: resolve ambiguity here by allowing only rel_pred or `binding_form`
# Can't use sel_expr because sel_expr has valcons or func, which looks exactly the same as predicates. 
pred_arg 
  -> rel_pred {% id %}
  |  binding_form {% ([d]) => ({...rangeFrom([d]), tag: "SEBind", contents: d}) %}

binding_form 
  -> subVar {% id %} 
  |  styVar {% id %}

# HACK: tokens like "`" don't really have start and end points, just line and col. How do you merge a heterogenrous list of tokens and nodes?
subVar -> "`" identifier "`" {%
  d => ({ ...rangeFrom([d[1]]), tag: "SubVar", contents: d[1]})
%}

styVar -> identifier {%
  d => ({ ...rangeFrom(d), tag: "StyVar", contents: d[0]})
%}

# NOTE: do not expect more ws after namespace because it's already parsing them for standalone use
select_as -> "as" __ namespace {% nth(2) %}

namespace -> identifier _ml {%
  (d): Namespace => ({
    ...rangeFrom([d[0]]),
    tag: "Namespace",
    contents: d[0]
  })
%}


################################################################################
# Block grammar

block -> "{" statements "}" {% 
  ([lbrace, stmts, rbrace]) => ({
    ...rangeBetween(lbrace, rbrace),
    statements: stmts
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
  -> delete {% id %}
  |  override {% id %}
  |  path_assign {% id %}
  |  anonymous_expr 
    {% ([d]) => ({...rangeOf(d), tag: "AnonAssign", contents: d}) %}

delete -> "delete" __ path {%
  (d) => {
   return {
    ...rangeBetween(d[0], d[2]),
    tag: "Delete",
    contents: d[2]
  }}
%}

override -> "override" __ path _ "=" _ assign_expr {%
  ([kw, , path, , , , expr]) => ({ 
    ...rangeBetween(kw, expr),
    tag: "Override",
    path, expr
  })
%}

path_assign -> type:? __ path _ "=" _ assign_expr {%
  ([type, , path, , , , expr]) => ({ 
    ...rangeBetween(type ? type : path, expr),
    tag: "PathAssign",
    type, path, expr
  })
%}

type 
  -> identifier {% ([d]): StyType => ({...rangeOf(d), tag: 'TypeOf', contents: d}) %}
  |  identifier "[]" {%
      ([d]): StyType => ({...rangeOf(d), tag: 'ListOf', contents: d}) 
     %}

path 
  -> entity_path   {% id %}
  |  access_path   {% id %}

entity_path
  -> propertyPath  {% id %}
  |  fieldPath     {% id %}
  |  localVar      {% id %}

propertyPath -> binding_form "." identifier "." identifier {%
  ([name, , field, , property]): IPropertyPath => ({
    ...rangeFrom([name, field, property]),
    tag: "PropertyPath",
    name, field, property
  })
%}

fieldPath -> binding_form "." identifier {%
  ([name, , field]): IFieldPath => ({
    ...rangeFrom([name, field]),
    tag: "FieldPath",
    name, field
  })
%}

localVar -> identifier {%
  ([d]): LocalVar => ({
    ...rangeFrom([d]),
    tag: "LocalVar",
    contents: d
  })
%}

# NOTE: not a subrule of entity_path so we can parse all indices into a list
# TODO: capture the range more accurately, now it's missing the last bracket
access_path -> entity_path _ access_ops {%
  ([path, , indices]): IAccessPath => {
    const lastIndex = last(indices);
    return {
      ...rangeBetween(path, lastIndex),
      tag: "AccessPath",
      path, indices
    }
  }
%}

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

# NOTE: inline computations on expr_literal (including expr_literal)
expr -> arithmeticExpr {% id %}

# Arith ops (NOTE: ordered in decreasing precedence)

# Parenthsized
parenthesized 
  -> "(" _ arithmeticExpr _ ")" {% nth(2) %}
  |  expr_literal               {% id %}

# Unary ops 
unary 
  -> "-" _ parenthesized  {% (d) => 
    ({
      ...rangeBetween(d[0], d[2]),
      tag: 'UOp', op: "UMinus", arg: d[2]
    }) 
  %}
  |  parenthesized        {% id %}

# Exponents
factor 
  -> unary _ "^" _ factor {% (d) => binop('Exp', d[0], d[4]) %}
  |  unary                {% id %}

# Multiplication and division
term 
  -> term _ "*" _ factor  {% (d) => binop('Multiply', d[0], d[4]) %}
  |  term _ "/" _ factor  {% (d) => binop('Divide', d[0], d[4]) %}
  |  factor               {% id %}

# Addition and subtraction
arithmeticExpr 
  -> arithmeticExpr _ "+" _ term {% (d) => binop('BPlus', d[0], d[4]) %}
  |  arithmeticExpr _ "-" _ term {% (d) => binop('BMinus', d[0], d[4]) %}
  |  term                        {% id %}

# NOTE: all of the expr_literal can be operands of inline computation 
expr_literal
  -> bool_lit {% id %}
  |  string_lit {% id %}
  |  annotated_float {% id %}
  |  computation_function {% id %}
  |  path {% id %}
  |  list {% id %}
  |  tuple {% id %}
  |  vector {% id %}
  # |  matrix {% id %} # NOTE: we liberally parse vectors to include the matrix case instead. All matrices are vectors of vectors.
  # TODO: 
  # |  transformExpr 

list -> "[" _ expr_list _ "]" {% 
  ([lbracket, , exprs, , rbracket]): IList => ({
    ...rangeBetween(lbracket, rbracket),
    tag: 'List',
    contents: exprs
  })
%}

# NOTE: we only allow 2 tuples and enforce this rule during parsing
tuple -> "{" _ expr _ "," _ expr _ "}" {% 
  ([lbrace, , e1, , , , e2, , rbrace]): ITuple => ({
    ...rangeBetween(lbrace, rbrace),
    tag: 'Tuple',
    contents: [e1, e2]
  })
%}

# NOTE: the extra expr makes sure a vector will always have >1 components.
vector -> "(" _ expr  _ "," expr_list ")" {% 
  ([lparen, , first, , , rest, rparen]): IVector => ({
    ...rangeBetween(lparen, rparen),
    tag: 'Vector',
    contents: [first, ...rest]
  })
%}

# NOTE: not used since `vector` will include this case. Actual type will be resolved by the compiler.
# matrix -> "(" _ sepBy1[vector, ","] _ ")" {% 
#   ([lparen, , exprs, , rparen]): IMatrix => ({
#     ...rangeBetween(lparen, rparen),
#     tag: 'Matrix',
#     contents: exprs
#   })
# %}

bool_lit -> ("true" | "false") {%
  ([[d]]): IBoolLit => ({
    ...rangeOf(d),
    tag: 'BoolLit',
    contents: d.text === 'true' // https://stackoverflow.com/questions/263965/how-can-i-convert-a-string-to-boolean-in-javascript
  })
%}

string_lit -> %string_literal {%
  ([d]): IStringLit => ({
    ...rangeOf(d),
    tag: 'StringLit',
    contents: JSON.parse(d.text)
  })
%}

annotated_float 
  -> "?" {% ([d]): IVary => ({ ...rangeOf(d), tag: 'Vary' }) %}
  |  %float_literal {% 
    ([d]): IFix => ({ ...rangeOf(d), tag: 'Fix', contents: parseFloat(d) }) 
  %}

layering
  -> layer_keyword:? path __ "below" __ path {% d => layering(d[0], d[1], d[5]) %}
  |  layer_keyword:? path __ "above" __ path {% d => layering(d[0], d[5], d[1]) %}

layer_keyword -> "layer" __ {% nth(0) %}

computation_function -> identifier _ "(" expr_list ")" {% 
  ([name, , , args, rparen]): ICompApp => ({
    ...rangeBetween(name, rparen),
    tag: "CompApp",
    name, args
  }) 
%}

objective -> "encourage" __ identifier _ "(" expr_list ")" {% 
  ([kw, , name, , , args, rparen]): IObjFn => ({
    ...rangeBetween(kw, rparen),
    tag: "ObjFn",
    name, args
  }) 
%}

constraint -> "ensure" __ identifier _ "(" expr_list ")" {% 
  ([kw, , name, , , args, rparen]): IConstrFn => ({
    ...rangeBetween(kw, rparen),
    tag: "ConstrFn",
    name, args
  }) 
%}

expr_list 
  -> _ {% d => [] %}
  |  _ sepBy1[expr, ","] _ {% nth(1) %}

gpi_decl -> identifier _ "{" property_decl_list "}" {%
  ([shapeName, , , properties, rbrace]): GPIDecl => ({
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
    |  _c_ "\n" property_decl_list {% nth(2) %} # 
    # spaces around each decl (NOTE: still wrap in list to spread later)
    |  _ property_decl _ {% d => [d[1]] %}
    # whitespaces in between and at the end (NOTE: comments are allowed)
    |  _ property_decl _c_ "\n" property_decl_list {% d => [d[1], ...d[4]] %}
  
property_decl -> identifier _ ":" _ expr {%
  ([name, , , , value]) => ({
    ...rangeBetween(name, value),
    tag: "PropertyDecl",
    name, value
  })
%}

# Common 

identifier -> %identifier {% 
  ([d]) => ({
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

multi_line_ws_char
    -> %ws
    |  "\n"
    | comment # skip comments

__ -> %ws:+ 

_ -> %ws:*
