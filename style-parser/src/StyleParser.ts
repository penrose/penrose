// Generated automatically by nearley, version 2.20.1
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d: any[]): any { return d[0]; }
declare var comment: any;
declare var identifier: any;
declare var ws: any;

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
const findRange = (nodes: any[]) => {
  // TODO: check if this heuristics always work out
  // console.log("find range", nodes);
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
  


interface NearleyToken {
  value: any;
  [key: string]: any;
};

interface NearleyLexer {
  reset: (chunk: string, info: any) => void;
  next: () => NearleyToken | undefined;
  save: () => any;
  formatError: (token: never) => string;
  has: (tokenType: string) => boolean;
};

interface NearleyRule {
  name: string;
  symbols: NearleySymbol[];
  postprocess?: (d: any[], loc?: number, reject?: {}) => any;
};

type NearleySymbol = string | { literal: any } | { test: (token: any) => boolean };

interface Grammar {
  Lexer: NearleyLexer | undefined;
  ParserRules: NearleyRule[];
  ParserStart: string;
};

const grammar: Grammar = {
  Lexer: lexer,
  ParserRules: [
    {"name": "input", "symbols": ["_ml", "header_blocks", "_ml"], "postprocess": res => res[1]},
    {"name": "header_blocks", "symbols": ["header", "_ml", "blocks"], "postprocess": 
        (d): HeaderBlock => ({
          ...findRange([d[0]]),
          //...findRange(d),
          tag:"HeaderBlock",
          header: d[0], 
          block: d[2]
        })
        },
    {"name": "header", "symbols": ["namespace"], "postprocess": id},
    {"name": "header", "symbols": ["selector"], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$1", "symbols": [{"literal":"forall"}], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1$ebnf$2", "symbols": ["select_where"], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1$ebnf$3", "symbols": ["select_with"], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$3", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1$ebnf$4", "symbols": ["select_as"], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$4", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1", "symbols": ["selector$subexpression$1$ebnf$1", "__", "decl_pattern", "_ml", "selector$subexpression$1$ebnf$2", "_ml", "selector$subexpression$1$ebnf$3", "_ml", "selector$subexpression$1$ebnf$4"]},
    {"name": "selector$subexpression$1$ebnf$5", "symbols": [{"literal":"forall"}], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$5", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1$ebnf$6", "symbols": ["select_with"], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$6", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1$ebnf$7", "symbols": ["select_where"], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$7", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1$ebnf$8", "symbols": ["select_as"], "postprocess": id},
    {"name": "selector$subexpression$1$ebnf$8", "symbols": [], "postprocess": () => null},
    {"name": "selector$subexpression$1", "symbols": ["selector$subexpression$1$ebnf$5", "__", "decl_pattern", "_ml", "selector$subexpression$1$ebnf$6", "_ml", "selector$subexpression$1$ebnf$7", "_ml", "selector$subexpression$1$ebnf$8"]},
    {"name": "selector", "symbols": ["selector$subexpression$1"], "postprocess": 
        ([d]) => { 
          return selector(d[2], d[4], d[6], d[8])
        } 
         },
    {"name": "select_with", "symbols": [{"literal":"with"}, "__", "decl_pattern"], "postprocess": d => d[2]},
    {"name": "decl_pattern$macrocall$2", "symbols": ["decl_list"]},
    {"name": "decl_pattern$macrocall$3", "symbols": [{"literal":";"}]},
    {"name": "decl_pattern$macrocall$1$ebnf$1", "symbols": []},
    {"name": "decl_pattern$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "decl_pattern$macrocall$3", "_", "decl_pattern$macrocall$2"]},
    {"name": "decl_pattern$macrocall$1$ebnf$1", "symbols": ["decl_pattern$macrocall$1$ebnf$1", "decl_pattern$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "decl_pattern$macrocall$1", "symbols": ["decl_pattern$macrocall$2", "decl_pattern$macrocall$1$ebnf$1"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map(ts => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "decl_pattern", "symbols": ["decl_pattern$macrocall$1"], "postprocess": d => concat(d)},
    {"name": "decl_list$macrocall$2", "symbols": ["binding_form"]},
    {"name": "decl_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "decl_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "decl_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "decl_list$macrocall$3", "_", "decl_list$macrocall$2"]},
    {"name": "decl_list$macrocall$1$ebnf$1", "symbols": ["decl_list$macrocall$1$ebnf$1", "decl_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "decl_list$macrocall$1", "symbols": ["decl_list$macrocall$2", "decl_list$macrocall$1$ebnf$1"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map(ts => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "decl_list", "symbols": ["identifier", "__", "decl_list$macrocall$1"], "postprocess":  
        ([type, , ids]) => {
          return declList(type, ids);
        }
        },
    {"name": "select_where", "symbols": [{"literal":"where"}, "__", "relation_list"], "postprocess": d => d[2]},
    {"name": "relation_list$macrocall$2", "symbols": ["relation"]},
    {"name": "relation_list$macrocall$3", "symbols": [{"literal":";"}]},
    {"name": "relation_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "relation_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "relation_list$macrocall$3", "_", "relation_list$macrocall$2"]},
    {"name": "relation_list$macrocall$1$ebnf$1", "symbols": ["relation_list$macrocall$1$ebnf$1", "relation_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "relation_list$macrocall$1", "symbols": ["relation_list$macrocall$2", "relation_list$macrocall$1$ebnf$1"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map(ts => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "relation_list", "symbols": ["relation_list$macrocall$1"], "postprocess": id},
    {"name": "relation", "symbols": ["rel_bind"], "postprocess": id},
    {"name": "relation", "symbols": ["rel_pred"], "postprocess": id},
    {"name": "rel_bind", "symbols": ["binding_form", "_", {"literal":":="}, "_", "sel_expr"], "postprocess": 
        ([id, , , , expr]) => ({
          ...findRange([id, expr]),
          tag: "RelBind",
          contents: [id, expr]
        })
        },
    {"name": "rel_pred", "symbols": ["identifier", "_", {"literal":"("}, "_", "pred_arg_list", "_", {"literal":")"}], "postprocess":  ([name, , , , args, , ,]) => ({
          ...findRange([name, ...args]),
          tag: "RelPred",
          name, args
        }) 
        },
    {"name": "sel_expr_list$macrocall$2", "symbols": ["sel_expr"]},
    {"name": "sel_expr_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "sel_expr_list$macrocall$1$ebnf$1", "symbols": ["sel_expr_list$macrocall$2"], "postprocess": id},
    {"name": "sel_expr_list$macrocall$1$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "sel_expr_list$macrocall$1$ebnf$2", "symbols": []},
    {"name": "sel_expr_list$macrocall$1$ebnf$2$subexpression$1", "symbols": ["_", "sel_expr_list$macrocall$3", "_", "sel_expr_list$macrocall$2"]},
    {"name": "sel_expr_list$macrocall$1$ebnf$2", "symbols": ["sel_expr_list$macrocall$1$ebnf$2", "sel_expr_list$macrocall$1$ebnf$2$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "sel_expr_list$macrocall$1", "symbols": ["sel_expr_list$macrocall$1$ebnf$1", "sel_expr_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(!first) return [];
          if(rest.length > 0) {
            const restNodes = rest.map(ts => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "sel_expr_list", "symbols": ["sel_expr_list$macrocall$1"], "postprocess": id},
    {"name": "sel_expr", "symbols": ["identifier", "_", {"literal":"("}, "_", "sel_expr_list", "_", {"literal":")"}], "postprocess":  ([name, , , , args, , ,]) => ({
          ...findRange([name, ...args]),
          tag: "SEFuncOrValCons",
          name, args
        }) 
          },
    {"name": "sel_expr", "symbols": ["binding_form"], "postprocess": ([d]) => ({...findRange([d]), tag: "SEBind", contents: d})},
    {"name": "pred_arg_list$macrocall$2", "symbols": ["pred_arg"]},
    {"name": "pred_arg_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "pred_arg_list$macrocall$1$ebnf$1", "symbols": ["pred_arg_list$macrocall$2"], "postprocess": id},
    {"name": "pred_arg_list$macrocall$1$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "pred_arg_list$macrocall$1$ebnf$2", "symbols": []},
    {"name": "pred_arg_list$macrocall$1$ebnf$2$subexpression$1", "symbols": ["_", "pred_arg_list$macrocall$3", "_", "pred_arg_list$macrocall$2"]},
    {"name": "pred_arg_list$macrocall$1$ebnf$2", "symbols": ["pred_arg_list$macrocall$1$ebnf$2", "pred_arg_list$macrocall$1$ebnf$2$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "pred_arg_list$macrocall$1", "symbols": ["pred_arg_list$macrocall$1$ebnf$1", "pred_arg_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(!first) return [];
          if(rest.length > 0) {
            const restNodes = rest.map(ts => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "pred_arg_list", "symbols": ["pred_arg_list$macrocall$1"], "postprocess": id},
    {"name": "pred_arg", "symbols": ["sel_expr"], "postprocess": id},
    {"name": "pred_arg", "symbols": ["rel_pred"], "postprocess": id},
    {"name": "binding_form", "symbols": ["subVar"], "postprocess": id},
    {"name": "binding_form", "symbols": ["styVar"], "postprocess": id},
    {"name": "subVar", "symbols": [{"literal":"`"}, "identifier", {"literal":"`"}], "postprocess": 
        d => ({ ...findRange([d[1]]), tag: "SubVar", contents: d[1]})
        },
    {"name": "styVar", "symbols": ["identifier"], "postprocess": 
        d => ({ ...findRange(d), tag: "StyVar", contents: d[0]})
        },
    {"name": "select_as", "symbols": [{"literal":"as"}, "__", "namespace"], "postprocess": d => d[2]},
    {"name": "namespace", "symbols": ["identifier"], "postprocess": 
        (d): Namespace => ({
          ...findRange(d),
          tag: "Namespace",
          contents: d[0]
        })
        },
    {"name": "blocks", "symbols": [{"literal":"{"}, "_ml", {"literal":"}"}], "postprocess": d => []},
    {"name": "statements", "symbols": ["statement"], "postprocess": 
        d => [d[0]]
                },
    {"name": "statements", "symbols": ["statement", "_", {"literal":"\n"}, "_", "statements"], "postprocess": 
        d => [
            d[0],
            ...d[4]
        ]
                },
    {"name": "statements", "symbols": ["_", {"literal":"\n"}, "statement"], "postprocess": 
        d => d[2]
                },
    {"name": "statements", "symbols": ["_"], "postprocess": 
        d => []
                },
    {"name": "statement", "symbols": ["line_comment"], "postprocess": id},
    {"name": "line_comment", "symbols": [(lexer.has("comment") ? {type: "comment"} : comment)], "postprocess": convertTokenId},
    {"name": "identifier", "symbols": [(lexer.has("identifier") ? {type: "identifier"} : identifier)], "postprocess": convertTokenId},
    {"name": "_ml$ebnf$1", "symbols": []},
    {"name": "_ml$ebnf$1", "symbols": ["_ml$ebnf$1", "multi_line_ws_char"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "_ml", "symbols": ["_ml$ebnf$1"]},
    {"name": "multi_line_ws_char", "symbols": [(lexer.has("ws") ? {type: "ws"} : ws)]},
    {"name": "multi_line_ws_char", "symbols": [{"literal":"\n"}]},
    {"name": "multi_line_ws_char", "symbols": ["line_comment"]},
    {"name": "__$ebnf$1", "symbols": [(lexer.has("ws") ? {type: "ws"} : ws)]},
    {"name": "__$ebnf$1", "symbols": ["__$ebnf$1", (lexer.has("ws") ? {type: "ws"} : ws)], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "__", "symbols": ["__$ebnf$1"]},
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", (lexer.has("ws") ? {type: "ws"} : ws)], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "_", "symbols": ["_$ebnf$1"]}
  ],
  ParserStart: "input",
};

export default grammar;
