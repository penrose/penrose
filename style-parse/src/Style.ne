@preprocessor typescript

@{%
import * as moo from "moo";
import { concat, compact, flatten } from 'lodash'

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
    identifier: {
        match: /[A-z_][A-Za-z_0-9]*/,
        type: moo.keywords({
            forall: "forall",
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
  return {
    token: token.text,
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
const astNode = (children: ASTNode[]) => {
  if(children.length === 0) throw new TypeError(`No children ${JSON.stringify(children)}`);
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

// const walkTree = <T>(root: ASTNode, f: (ASTNode): T) => {
  // TODO: implement
// }

function convertTokenId(data) {
    return tokenRange(data[0]);
}

const declList = (type, ids) => {
  const decl = (t, i) => ({
    ...astNode([t, i]),
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
    ...astNode(compact([hd, wth, whr, namespace])),
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

# Grammar

input -> _ml header_blocks {% res => res[1] %}

header_blocks -> (header_block):* {%
  (d) => {
    const blocks = d;
    return {
      ...astNode(blocks),
      tag: "StyProg",
      contents: blocks
    }
  }
%}

header_block -> header block _ml {%
 (d): HeaderBlock => ({
   // TODO: range
   ...astNode([d[0]]),
   tag:"HeaderBlock",
   header: d[0], 
   block: d[1]
 })
%}

# selector
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
      ...astNode(contents),
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
    ...astNode(d),
    tag: "RelationPatterns",
    contents: d
  })
%}

relation
  -> rel_bind {% id %} 
  |  rel_pred {% id %}

rel_bind -> binding_form _ ":=" _ sel_expr {%
  ([id, , , , expr]) => ({
    ...astNode([id, expr]),
    tag: "RelBind",
    id, expr
  })
%}

rel_pred -> identifier _ "(" pred_arg_list ")" {% ([name, , , args, ]) => ({
      ...astNode([name, ...args]),
      tag: "RelPred",
      name, args
    }) 
%}

sel_expr_list 
  -> _ {% d => [] %}
  |  _ sepBy1[sel_expr, ","] _ {% nth(1) %}

sel_expr 
  -> identifier _ "(" sel_expr_list ")" {% ([name, , , args, ]) => ({
      ...astNode([name, ...args]),
      tag: "SEFuncOrValCons",
      name, args
    }) 
  %}
  |  binding_form {% ([d]) => ({...astNode([d]), tag: "SEBind", contents: d}) %}


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
  d => ({ ...astNode([d[1]]), tag: "SubVar", contents: d[1]})
%}

styVar -> identifier {%
  d => ({ ...astNode(d), tag: "StyVar", contents: d[0]})
%}

# NOTE: do not expect more ws after namespace because it's already parsing them for standalone use
select_as -> "as" __ namespace {% nth(2) %}

namespace -> identifier _ml {%
  (d): Namespace => ({
    ...astNode([d[0]]),
    tag: "Namespace",
    contents: d[0]
  })
%}


# block

block -> "{" _ml "}" {% d => [] %}

statements
    ->  statement
        {%
            d => [d[0]]
        %}
    |  statement _ "\n" _ statements
        {%
            d => [
                d[0],
                ...d[4]
            ]
        %}
    # below 2 sub-rules handle blank lines
    |  _ "\n" statement
        {%
            d => d[2]
        %}
    |  _
        {%
            d => []
        %}


statement
    -> line_comment     {% id %}

line_comment -> %comment {% convertTokenId %}

identifier -> %identifier {% convertTokenId %}

# white space definitions 

_ml -> multi_line_ws_char:* 

multi_line_ws_char
    -> %ws
    |  "\n"
    | line_comment # skip comments

__ -> %ws:+

_ -> %ws:*
