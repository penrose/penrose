@preprocessor typescript

@{%
import * as moo from "moo";
import { concat } from 'lodash'

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
    plus: "+",
    multiply: "*",
    divide: "/",
    modulo: "%",
    colon: ":",
    semi: ";",
    tick: "`",
    comment: /--.*?$/,
    identifier: {
        match: /[A-z_][a-z_0-9]*/,
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
const findRange = (nodes: any[]) => {
  // TODO: check if this heuristics always work out
  if(nodes.length === 0) throw new TypeError();
  if(nodes.length === 1) {
    return { start: nodes[0].start, end: nodes[0].end };
  }

  return {
    start: minLoc(...nodes.map(n => n.start)),
    end: maxLoc(...nodes.map(n => n.end)),
  }
}

function convertTokenId(data) {
    return tokenRange(data[0]);
}

const declList = (type, ids) => {
  // TODO: range for subsequent ids
  const decl = (t, i) => ({
    ...findRange([t, i]),
    tag: "DeclPattern",
    type: t,
    id: i 
  });
  return ids.map(i => decl(type, i));
}

const selector = (
  hd: DeclPattern[],
  wth?: DeclPattern[],
  whr?: RelationPattern[],
  namespace?: Namespace
): Selector => {
  return {
    // TODO: range
    // ...findRange([...hd, ...wth, ...whr, namespace]),
    ...findRange(hd),
    tag: "Selector",
    head: hd,
    with: wth,
    where: whr,
    namespace,
  };
}
  

%}

@lexer lexer

# Macro

sepBy[ITEM, SEP] -> $ITEM (_ $SEP _ $ITEM):* {% 
  d => { 
    const [first, rest] = [d[0], d[1]];
    if(rest.length > 0) {
      const restNodes = rest.map(ts => ts[3]);
      return concat(first, ...restNodes);
    } else return first;
  }
%}

sepBy1[ITEM, SEP] -> $ITEM (_ $SEP _ $ITEM):+

input -> _ml header_blocks _ml {% res => res[1] %}

header_blocks -> header _ml blocks {%
 (d): HeaderBlock => ({
   ...findRange([d[0]]),
   //...findRange(d),
   tag:"HeaderBlock",
   header: d[0], 
   block: d[2]
 })
%}

# selector
header -> namespace {% id %}
      | selector {% id %}

selector -> 
  ( "forall":? __ decl_pattern _ml select_where:? _ml select_with:? _ml select_as:? 
  | "forall":? __ decl_pattern _ml select_where:? _ml select_with:? _ml select_as:?) {%
  ([d]) => { 
    console.log(d[2], d[4], d[6], d[8]);
    return selector(d[2], d[4], d[6], d[8])
  } 
 %}

select_with -> "with" __ decl_pattern {% d => d[2] %}

# TODO: abstract this pattern out?
decl_pattern -> sepBy[decl_list, ";"]  {% d => concat(d) %}

decl_list -> identifier __ sepBy[binding_form, ","] {% 
  ([type, , ids]) => {
    return declList(type, ids);
  }
%}

select_where -> "where" __ relation_patterns {% d => d[2] %}

relation_patterns 
  -> rel_bind {% id %} 
  |  rel_pred {% id %}

rel_bind -> binding_form _ ":=" _ sel_expr {%
  ([id, , , , expr]) => ({
    ...findRange([id, expr]),
    tag: "RelBind",
    contents: [id, expr]
  })
%}

# TODO: complete
sel_expr 
  -> binding_form {%
      ([d]) => ({...findRange(d), tag: "SEBind", contents: d}) 
     %}

# sel_expr_list -> sel_expr (_ "," _ sel_expr)


binding_form 
  -> subVar {% id %} 
  |  styVar {% id %}

# HACK: tokens like "`" don't really have start and end points, just line and col. How do you merge a heterogenrous list of tokens and nodes?
subVar -> "`" identifier "`" {%
  d => ({ ...findRange([d[1]]), tag: "SubVar", contents: d[1]})
%}

styVar -> identifier {%
  d => ({ ...findRange(d), tag: "StyVar", contents: d[0]})
%}

select_as -> "as" __ namespace {% d => d[2] %}

namespace -> identifier {%
  (d): Namespace => ({
    ...findRange(d),
    tag: "Namespace",
    contents: d[0]
  })
%}


# block

blocks -> "{" _ml "}" {% d => [] %}

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
