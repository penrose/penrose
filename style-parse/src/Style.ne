@preprocessor typescript

@{%
import * as moo from "moo";
import { concat, compact, flatten } from 'lodash'

const styleTypes: string[] =
  [ "scalar"
  , "int"
  , "bool"
  , "string"
  , "path"
  , "color"
  , "file"
  , "style"
  , "shape"
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
const lexer = moo.compile({
    ws: /[ \t]+/,
    nl: { match: "\n", lineBreaks: true },
    lte: "<=",
    lt: "<",
    gte: ">=",
    gt: ">",
    eq: "==",
    lparan: "(",
    rparan: ")",
    comma: ",",
    dot: ".",
    brackets: "[]",
    lbracket: "[",
    rbracket: "]",
    lbrace: "{",
    rbrace: "}",
    assignment: "=",
    def: ":=",
    plus: "+",
    multiply: "*",
    divide: "/",
    modulo: "%",
    colon: ":",
    semi: ";",
    tick: "\`",
    comment: /--.*?$/,
    string_literal: /"(?:[^\n\\"]|\\["\\ntbfr])*"/,
    identifier: {
        match: /[A-z_][A-Za-z_0-9]*/,
        type: moo.keywords({
            ...Object.fromEntries(styleTypes.map(k => ['typeword' + k, k])),
            forall: "forall",
            where: "where",
            with: "with",
            delete: "delete",
            as: "as",
            true: "true",
            false: "false",
        })
    }
});


function tokenStart(token) {
  return {
      line: token.line,
      col: token.col - 1
  };
}

function tokenEnd(token) {
    const lastNewLine = token.text.lastIndexOf("\n");
    if (lastNewLine !== -1) {
        throw new Error("Unsupported case: token with line breaks");
    }
    return {
        line: token.line,
        col: token.col + token.text.length - 1
    };
}

const tokenRange = (token: any) => {
  // if it's already converted, assume it's an AST Node
  if(token.start) return token;
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
const rangeOf = (children: ASTNode[]) => {
  // TODO: this function is called in intermediate steps with empty lists, so will need to guard against empty lists.
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

const rangeBetween = (beginToken, endToken) => {
  const [beginRange, endRange] = [beginToken, endToken].map(tokenRange);
  return {
    start: beginRange.start,
    end: endRange.end
  }
}

// const walkTree = <T>(root: ASTNode, f: (ASTNode): T) => {
  // TODO: implement
// }

const convertTokenId =([token]) => {
  return {
    ...tokenRange(token),
    token: token.text,
    type: token.type,
  };
}

const declList = (type, ids) => {
  const decl = (t, i) => ({
    ...rangeOf([t, i]),
    tag: "DeclPattern",
    type: t,
    id: i 
  });
  return ids.map(i => decl(type, i));
}

const selector = (
  hd: DeclPatterns,
  wth?: DeclPatterns,
  whr?: RelationPatterns,
  namespace?: Namespace
): Selector => {
  return {
    ...rangeOf(compact([hd, wth, whr, namespace])),
    tag: "Selector",
    head: hd,
    with: wth,
    where: whr,
    namespace,
  };
}

function nth(n) {
    return function(d) {
        return d[n];
    };
}
%}

@lexer lexer

# Macros

# TODO: factor out sepEndBy
sepBy1[ITEM, SEP] -> $ITEM (_ $SEP _ $ITEM):* $SEP:? {% 
  d => { 
    const [first, rest] = [d[0], d[1]];
    if(rest.length > 0) {
      const restNodes = rest.map(ts => ts[3]);
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
    ...rangeOf(blocks),
    tag: "StyProg",
    contents: blocks
  })
%}

header_blocks -> header_block:* {% id %}

header_block -> header block _ml {%
 ([header, block]): HeaderBlock => ({
   ...rangeOf([header, block]), tag:"HeaderBlock", header, block })
%}

################################################################################
# Selector grammar

header 
  -> namespace {% id %}
  |  selector  {% id %}

selector -> 
    "forall":? __ decl_patterns _ml select_as:?  
    {% (d) => selector(d[2], null, null, d[4]) %} 
  | "forall":? __ decl_patterns _ml select_where select_as:? 
    {% (d) => selector(d[2], null, d[4], d[5]) %} 
  | "forall":? __ decl_patterns _ml select_with select_as:? 
    {% (d) => selector(d[2], d[4], null, d[5]) %} 
  | "forall":? __ decl_patterns _ml select_where select_with select_as:? 
    {% (d) => selector(d[2], d[5], d[4], d[6]) %} 
  | "forall":? __ decl_patterns _ml select_with select_where select_as:? 
    {% (d) => selector(d[2], d[4], d[5], d[6]) %}

select_with -> "with" __ decl_patterns _ml {% d => d[2] %}

decl_patterns -> sepBy1[decl_list, ";"] {% 
  ([d]) => {
    const contents = flatten(d);
    return {
      ...rangeOf(contents),
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
    ...rangeOf(d),
    tag: "RelationPatterns",
    contents: d
  })
%}

relation
  -> rel_bind {% id %} 
  |  rel_pred {% id %}

rel_bind -> binding_form _ ":=" _ sel_expr {%
  ([id, , , , expr]) => ({
    ...rangeOf([id, expr]),
    tag: "RelBind",
    id, expr
  })
%}

rel_pred -> identifier _ "(" pred_arg_list ")" {% 
  ([name, , , args, ]) => ({
    ...rangeOf([name, ...args]),
    tag: "RelPred",
    name, args
  }) 
%}

sel_expr_list 
  -> _ {% d => [] %}
  |  _ sepBy1[sel_expr, ","] _ {% nth(1) %}

sel_expr 
  -> identifier _ "(" sel_expr_list ")" {% ([name, , , args, ]) => ({
      ...rangeOf([name, ...args]),
      tag: "SEFuncOrValCons",
      name, args
    }) 
  %}
  |  binding_form {% ([d]) => ({...rangeOf([d]), tag: "SEBind", contents: d}) %}


pred_arg_list 
  -> _ {% d => [] %}
  |  _ sepBy1[pred_arg, ","] _ {% nth(1) %}

# NOTE: ambiguity here because sel_expr has valcons or func, which looks exactly the same as predicates. 
pred_arg 
  -> rel_pred {% id %}
  |  binding_form {% id %}

binding_form 
  -> subVar {% id %} 
  |  styVar {% id %}

# HACK: tokens like "`" don't really have start and end points, just line and col. How do you merge a heterogenrous list of tokens and nodes?
subVar -> "`" identifier "`" {%
  d => ({ ...rangeOf([d[1]]), tag: "SubVar", contents: d[1]})
%}

styVar -> identifier {%
  d => ({ ...rangeOf(d), tag: "StyVar", contents: d[0]})
%}

# NOTE: do not expect more ws after namespace because it's already parsing them for standalone use
select_as -> "as" __ namespace {% nth(2) %}

namespace -> identifier _ml {%
  (d): Namespace => ({
    ...rangeOf([d[0]]),
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
  |  path_assign {% id %}

delete -> "delete" __ path {%
  (d) => {
   return {
    ...rangeBetween(d[0], d[2]),
    tag: "Delete",
    contents: d[2]
  }}
%}

path_assign -> type:? __ path _ "=" _ expr {%
  ([type, , path, , , , expr]) => ({
    ...rangeBetween(type ? type : path, expr),
    tag: "PathAssign",
    type, path, expr
  })
%}

type -> identifier {% id %}

# | identifier "[]"

path 
  -> propertyPath {% id %}
  |  fieldPath    {% id %}
  |  localVar     {% id %}


propertyPath -> binding_form "." identifier "." identifier {%
  ([sub, , field, , prop]) => ({
    ...rangeOf([sub, field, prop]),
    tag: "PropertyPath",
    contents: [sub, field, prop]
  })
%}

fieldPath -> binding_form "." identifier {%
  ([sub, , field]) => ({
    ...rangeOf([sub, field]),
    tag: "FieldPath",
    contents: [sub, field]
  })
%}

localVar -> identifier {%
  ([d]) => ({
    ...rangeOf([d]),
    tag: "LocalVar",
    contents: d
  })
%}

# Expression

expr 
  -> bool_lit 
  |  string_lit
  # |  constructor 
  # |  layeringExpr
  # |  objFn
  # |  constrFn
  # |  transformExpr 
  # |  compFn
  # |  list
  # |  tuple
  # |  vector
  # |  arithmeticExpr

bool_lit -> "true" | "false" {%
  ([d]): IBoolLit => ({
    ...tokenRange(d),
    tag: 'BoolLit',
    contents: d.text === 'true' // https://stackoverflow.com/questions/263965/how-can-i-convert-a-string-to-boolean-in-javascript
  })
%}

string_lit -> %string_literal {%
  ([d]): IStringLit => ({
    ...tokenRange(d),
    tag: 'StringLit',
    contents: d.text
  })
%}

# Common 

identifier -> %identifier {% convertTokenId %}

# white space definitions 
# TODO: multiline comments

line_comment -> %comment {% convertTokenId %}

_c_ -> (%ws | line_comment):* 

_ml -> multi_line_ws_char:* 

multi_line_ws_char
    -> %ws
    |  "\n"
    | line_comment # skip comments

__ -> %ws:+ 

_ -> %ws:*
