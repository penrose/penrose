// Generated automatically by nearley, version 2.20.1
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d: any[]): any { return d[0]; }
declare var string_literal: any;
declare var float_literal: any;
declare var identifier: any;
declare var comment: any;
declare var multiline_comment: any;
declare var ws: any;


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
    {"name": "input", "symbols": ["_ml", "header_blocks"], "postprocess": 
        ([, blocks]) => ({
          ...rangeFrom(blocks),
          tag: "StyProg",
          blocks
        })
        },
    {"name": "header_blocks$ebnf$1", "symbols": []},
    {"name": "header_blocks$ebnf$1", "symbols": ["header_blocks$ebnf$1", "header_block"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "header_blocks", "symbols": ["header_blocks$ebnf$1"], "postprocess": id},
    {"name": "header_block", "symbols": ["header", "block", "_ml"], "postprocess": 
        ([header, block]): HeaderBlock => ({
          ...rangeFrom([header, block]), tag:"HeaderBlock", header, block })
        },
    {"name": "header", "symbols": ["selector"], "postprocess": id},
    {"name": "header", "symbols": ["namespace"], "postprocess": id},
    {"name": "selector$ebnf$1", "symbols": ["forall"], "postprocess": id},
    {"name": "selector$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "selector$ebnf$2", "symbols": ["select_as"], "postprocess": id},
    {"name": "selector$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "selector", "symbols": ["selector$ebnf$1", "decl_patterns", "_ml", "selector$ebnf$2"], "postprocess": (d) => selector(d[1], undefined, undefined, d[4])},
    {"name": "selector$ebnf$3", "symbols": ["forall"], "postprocess": id},
    {"name": "selector$ebnf$3", "symbols": [], "postprocess": () => null},
    {"name": "selector$ebnf$4", "symbols": ["select_as"], "postprocess": id},
    {"name": "selector$ebnf$4", "symbols": [], "postprocess": () => null},
    {"name": "selector", "symbols": ["selector$ebnf$3", "decl_patterns", "_ml", "select_where", "selector$ebnf$4"], "postprocess": (d) => selector(d[1], undefined, d[4], d[5])},
    {"name": "selector$ebnf$5", "symbols": ["forall"], "postprocess": id},
    {"name": "selector$ebnf$5", "symbols": [], "postprocess": () => null},
    {"name": "selector$ebnf$6", "symbols": ["select_as"], "postprocess": id},
    {"name": "selector$ebnf$6", "symbols": [], "postprocess": () => null},
    {"name": "selector", "symbols": ["selector$ebnf$5", "decl_patterns", "_ml", "select_with", "selector$ebnf$6"], "postprocess": (d) => selector(d[1], d[4], undefined, d[5])},
    {"name": "selector$ebnf$7", "symbols": ["forall"], "postprocess": id},
    {"name": "selector$ebnf$7", "symbols": [], "postprocess": () => null},
    {"name": "selector$ebnf$8", "symbols": ["select_as"], "postprocess": id},
    {"name": "selector$ebnf$8", "symbols": [], "postprocess": () => null},
    {"name": "selector", "symbols": ["selector$ebnf$7", "decl_patterns", "_ml", "select_where", "select_with", "selector$ebnf$8"], "postprocess": (d) => selector(d[1], d[5], d[4], d[6])},
    {"name": "selector$ebnf$9", "symbols": ["forall"], "postprocess": id},
    {"name": "selector$ebnf$9", "symbols": [], "postprocess": () => null},
    {"name": "selector$ebnf$10", "symbols": ["select_as"], "postprocess": id},
    {"name": "selector$ebnf$10", "symbols": [], "postprocess": () => null},
    {"name": "selector", "symbols": ["selector$ebnf$9", "decl_patterns", "_ml", "select_with", "select_where", "selector$ebnf$10"], "postprocess": (d) => selector(d[1], d[4], d[5], d[6])},
    {"name": "forall", "symbols": [{"literal":"forall"}, "__"], "postprocess": nth(0)},
    {"name": "select_with", "symbols": [{"literal":"with"}, "__", "decl_patterns", "_ml"], "postprocess": d => d[2]},
    {"name": "decl_patterns$macrocall$2", "symbols": ["decl_list"]},
    {"name": "decl_patterns$macrocall$3", "symbols": [{"literal":";"}]},
    {"name": "decl_patterns$macrocall$1$ebnf$1", "symbols": []},
    {"name": "decl_patterns$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "decl_patterns$macrocall$3", "_", "decl_patterns$macrocall$2"]},
    {"name": "decl_patterns$macrocall$1$ebnf$1", "symbols": ["decl_patterns$macrocall$1$ebnf$1", "decl_patterns$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "decl_patterns$macrocall$1$ebnf$2", "symbols": ["decl_patterns$macrocall$3"], "postprocess": id},
    {"name": "decl_patterns$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "decl_patterns$macrocall$1", "symbols": ["decl_patterns$macrocall$2", "decl_patterns$macrocall$1$ebnf$1", "decl_patterns$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "decl_patterns", "symbols": ["decl_patterns$macrocall$1"], "postprocess":  
        ([d]) => {
          const contents = flatten(d) as ASTNode[];
          return {
            ...rangeFrom(contents),
            tag: "DeclPatterns", contents
          };
        }
        },
    {"name": "decl_list$macrocall$2", "symbols": ["binding_form"]},
    {"name": "decl_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "decl_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "decl_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "decl_list$macrocall$3", "_", "decl_list$macrocall$2"]},
    {"name": "decl_list$macrocall$1$ebnf$1", "symbols": ["decl_list$macrocall$1$ebnf$1", "decl_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "decl_list$macrocall$1$ebnf$2", "symbols": ["decl_list$macrocall$3"], "postprocess": id},
    {"name": "decl_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "decl_list$macrocall$1", "symbols": ["decl_list$macrocall$2", "decl_list$macrocall$1$ebnf$1", "decl_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "decl_list", "symbols": ["identifier", "__", "decl_list$macrocall$1"], "postprocess":  
        ([type, , ids]) => {
          return declList(type, ids);
        }
        },
    {"name": "select_where", "symbols": [{"literal":"where"}, "__", "relation_list", "_ml"], "postprocess": d => d[2]},
    {"name": "relation_list$macrocall$2", "symbols": ["relation"]},
    {"name": "relation_list$macrocall$3", "symbols": [{"literal":";"}]},
    {"name": "relation_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "relation_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "relation_list$macrocall$3", "_", "relation_list$macrocall$2"]},
    {"name": "relation_list$macrocall$1$ebnf$1", "symbols": ["relation_list$macrocall$1$ebnf$1", "relation_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "relation_list$macrocall$1$ebnf$2", "symbols": ["relation_list$macrocall$3"], "postprocess": id},
    {"name": "relation_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "relation_list$macrocall$1", "symbols": ["relation_list$macrocall$2", "relation_list$macrocall$1$ebnf$1", "relation_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "relation_list", "symbols": ["relation_list$macrocall$1"], "postprocess":  ([d]) => ({
          ...rangeFrom(d),
          tag: "RelationPatterns",
          contents: d
        })
        },
    {"name": "relation", "symbols": ["rel_bind"], "postprocess": id},
    {"name": "relation", "symbols": ["rel_pred"], "postprocess": id},
    {"name": "rel_bind", "symbols": ["binding_form", "_", {"literal":":="}, "_", "sel_expr"], "postprocess": 
        ([id, , , , expr]) => ({
          ...rangeFrom([id, expr]),
          tag: "RelBind",
          id, expr
        })
        },
    {"name": "rel_pred", "symbols": ["identifier", "_", {"literal":"("}, "pred_arg_list", {"literal":")"}], "postprocess":  
        ([name, , , args, ]) => ({
          ...rangeFrom([name, ...args]),
          tag: "RelPred",
          name, args
        }) 
        },
    {"name": "sel_expr_list", "symbols": ["_"], "postprocess": d => []},
    {"name": "sel_expr_list$macrocall$2", "symbols": ["sel_expr"]},
    {"name": "sel_expr_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "sel_expr_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "sel_expr_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "sel_expr_list$macrocall$3", "_", "sel_expr_list$macrocall$2"]},
    {"name": "sel_expr_list$macrocall$1$ebnf$1", "symbols": ["sel_expr_list$macrocall$1$ebnf$1", "sel_expr_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "sel_expr_list$macrocall$1$ebnf$2", "symbols": ["sel_expr_list$macrocall$3"], "postprocess": id},
    {"name": "sel_expr_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "sel_expr_list$macrocall$1", "symbols": ["sel_expr_list$macrocall$2", "sel_expr_list$macrocall$1$ebnf$1", "sel_expr_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "sel_expr_list", "symbols": ["_", "sel_expr_list$macrocall$1", "_"], "postprocess": nth(1)},
    {"name": "sel_expr", "symbols": ["identifier", "_", {"literal":"("}, "sel_expr_list", {"literal":")"}], "postprocess":  ([name, , , args, ]) => ({
          ...rangeFrom([name, ...args]),
          tag: "SEFuncOrValCons",
          name, args
        }) 
          },
    {"name": "sel_expr", "symbols": ["binding_form"], "postprocess": ([d]) => ({...rangeFrom([d]), tag: "SEBind", contents: d})},
    {"name": "pred_arg_list", "symbols": ["_"], "postprocess": d => []},
    {"name": "pred_arg_list$macrocall$2", "symbols": ["pred_arg"]},
    {"name": "pred_arg_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "pred_arg_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "pred_arg_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "pred_arg_list$macrocall$3", "_", "pred_arg_list$macrocall$2"]},
    {"name": "pred_arg_list$macrocall$1$ebnf$1", "symbols": ["pred_arg_list$macrocall$1$ebnf$1", "pred_arg_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "pred_arg_list$macrocall$1$ebnf$2", "symbols": ["pred_arg_list$macrocall$3"], "postprocess": id},
    {"name": "pred_arg_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "pred_arg_list$macrocall$1", "symbols": ["pred_arg_list$macrocall$2", "pred_arg_list$macrocall$1$ebnf$1", "pred_arg_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "pred_arg_list", "symbols": ["_", "pred_arg_list$macrocall$1", "_"], "postprocess": nth(1)},
    {"name": "pred_arg", "symbols": ["rel_pred"], "postprocess": id},
    {"name": "pred_arg", "symbols": ["binding_form"], "postprocess": id},
    {"name": "binding_form", "symbols": ["subVar"], "postprocess": id},
    {"name": "binding_form", "symbols": ["styVar"], "postprocess": id},
    {"name": "subVar", "symbols": [{"literal":"`"}, "identifier", {"literal":"`"}], "postprocess": 
        d => ({ ...rangeFrom([d[1]]), tag: "SubVar", contents: d[1]})
        },
    {"name": "styVar", "symbols": ["identifier"], "postprocess": 
        d => ({ ...rangeFrom(d), tag: "StyVar", contents: d[0]})
        },
    {"name": "select_as", "symbols": [{"literal":"as"}, "__", "namespace"], "postprocess": nth(2)},
    {"name": "namespace", "symbols": ["identifier", "_ml"], "postprocess": 
        (d): Namespace => ({
          ...rangeFrom([d[0]]),
          tag: "Namespace",
          contents: d[0]
        })
        },
    {"name": "block", "symbols": [{"literal":"{"}, "statements", {"literal":"}"}], "postprocess":  
        ([lbrace, stmts, rbrace]) => ({
          ...rangeBetween(lbrace, rbrace),
          statements: stmts
        })
        },
    {"name": "statements", "symbols": ["_"], "postprocess": () => []},
    {"name": "statements", "symbols": ["_c_", {"literal":"\n"}, "statements"], "postprocess": nth(2)},
    {"name": "statements", "symbols": ["_", "statement", "_"], "postprocess": d => [d[1]]},
    {"name": "statements", "symbols": ["_", "statement", "_c_", {"literal":"\n"}, "statements"], "postprocess": d => [d[1], ...d[4]]},
    {"name": "statement", "symbols": ["delete"], "postprocess": id},
    {"name": "statement", "symbols": ["override"], "postprocess": id},
    {"name": "statement", "symbols": ["path_assign"], "postprocess": id},
    {"name": "statement", "symbols": ["anonymous_expr"], "postprocess": ([d]) => ({...rangeOf(d), tag: "AnonAssign", contents: d})},
    {"name": "delete", "symbols": [{"literal":"delete"}, "__", "path"], "postprocess": 
        (d) => {
         return {
          ...rangeBetween(d[0], d[2]),
          tag: "Delete",
          contents: d[2]
        }}
        },
    {"name": "override", "symbols": [{"literal":"override"}, "__", "path", "_", {"literal":"="}, "_", "assign_expr"], "postprocess": 
        ([kw, , path, , , , expr]) => ({ 
          ...rangeBetween(kw, expr),
          tag: "Override",
          path, expr
        })
        },
    {"name": "path_assign$ebnf$1", "symbols": ["type"], "postprocess": id},
    {"name": "path_assign$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "path_assign", "symbols": ["path_assign$ebnf$1", "__", "path", "_", {"literal":"="}, "_", "assign_expr"], "postprocess": 
        ([type, , path, , , , expr]) => ({ 
          ...rangeBetween(type ? type : path, expr),
          tag: "PathAssign",
          type, path, expr
        })
        },
    {"name": "type", "symbols": ["identifier"], "postprocess": ([d]): StyType => ({...rangeOf(d), tag: 'TypeOf', contents: d})},
    {"name": "type", "symbols": ["identifier", {"literal":"[]"}], "postprocess": 
        ([d]): StyType => ({...rangeOf(d), tag: 'ListOf', contents: d}) 
             },
    {"name": "path", "symbols": ["entity_path"], "postprocess": id},
    {"name": "path", "symbols": ["access_path"], "postprocess": id},
    {"name": "entity_path", "symbols": ["propertyPath"], "postprocess": id},
    {"name": "entity_path", "symbols": ["fieldPath"], "postprocess": id},
    {"name": "entity_path", "symbols": ["localVar"], "postprocess": id},
    {"name": "propertyPath", "symbols": ["binding_form", {"literal":"."}, "identifier", {"literal":"."}, "identifier"], "postprocess": 
        ([name, , field, , property]): IPropertyPath => ({
          ...rangeFrom([name, field, property]),
          tag: "PropertyPath",
          name, field, property
        })
        },
    {"name": "fieldPath", "symbols": ["binding_form", {"literal":"."}, "identifier"], "postprocess": 
        ([name, , field]): IFieldPath => ({
          ...rangeFrom([name, field]),
          tag: "FieldPath",
          name, field
        })
        },
    {"name": "localVar", "symbols": ["identifier"], "postprocess": 
        ([d]): LocalVar => ({
          ...rangeFrom([d]),
          tag: "LocalVar",
          contents: d
        })
        },
    {"name": "access_path", "symbols": ["entity_path", "_", "access_ops"], "postprocess": 
        ([path, , indices]): IAccessPath => {
          const lastIndex = last(indices);
          return {
            ...rangeBetween(path, lastIndex),
            tag: "AccessPath",
            path, indices
          }
        }
        },
    {"name": "access_ops", "symbols": ["access_op"]},
    {"name": "access_ops", "symbols": ["access_op", "_", "access_ops"], "postprocess": (d: any[]) => [d[0], ...d[2]]},
    {"name": "access_op", "symbols": [{"literal":"["}, "_", "expr", "_", {"literal":"]"}], "postprocess": nth(2)},
    {"name": "assign_expr", "symbols": ["expr"], "postprocess": id},
    {"name": "assign_expr", "symbols": ["layering"], "postprocess": id},
    {"name": "assign_expr", "symbols": ["objective"], "postprocess": id},
    {"name": "assign_expr", "symbols": ["constraint"], "postprocess": id},
    {"name": "assign_expr", "symbols": ["gpi_decl"], "postprocess": id},
    {"name": "anonymous_expr", "symbols": ["layering"], "postprocess": id},
    {"name": "anonymous_expr", "symbols": ["objective"], "postprocess": id},
    {"name": "anonymous_expr", "symbols": ["constraint"], "postprocess": id},
    {"name": "expr", "symbols": ["arithmeticExpr"], "postprocess": id},
    {"name": "parenthesized", "symbols": [{"literal":"("}, "_", "arithmeticExpr", "_", {"literal":")"}], "postprocess": nth(2)},
    {"name": "parenthesized", "symbols": ["expr_literal"], "postprocess": id},
    {"name": "unary", "symbols": [{"literal":"-"}, "_", "parenthesized"], "postprocess":  (d) => 
        ({
          ...rangeBetween(d[0], d[2]),
          tag: 'UOp', op: "UMinus", arg: d[2]
        }) 
          },
    {"name": "unary", "symbols": ["parenthesized"], "postprocess": id},
    {"name": "factor", "symbols": ["unary", "_", {"literal":"^"}, "_", "factor"], "postprocess": (d) => binop('Exp', d[0], d[4])},
    {"name": "factor", "symbols": ["unary"], "postprocess": id},
    {"name": "term", "symbols": ["term", "_", {"literal":"*"}, "_", "factor"], "postprocess": (d) => binop('Multiply', d[0], d[4])},
    {"name": "term", "symbols": ["term", "_", {"literal":"/"}, "_", "factor"], "postprocess": (d) => binop('Divide', d[0], d[4])},
    {"name": "term", "symbols": ["factor"], "postprocess": id},
    {"name": "arithmeticExpr", "symbols": ["arithmeticExpr", "_", {"literal":"+"}, "_", "term"], "postprocess": (d) => binop('BPlus', d[0], d[4])},
    {"name": "arithmeticExpr", "symbols": ["arithmeticExpr", "_", {"literal":"-"}, "_", "term"], "postprocess": (d) => binop('BMinus', d[0], d[4])},
    {"name": "arithmeticExpr", "symbols": ["term"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["bool_lit"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["string_lit"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["annotated_float"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["computation_function"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["path"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["list"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["tuple"], "postprocess": id},
    {"name": "expr_literal", "symbols": ["vector"], "postprocess": id},
    {"name": "list", "symbols": [{"literal":"["}, "_", "expr_list", "_", {"literal":"]"}], "postprocess":  
        ([lbracket, , exprs, , rbracket]): IList => ({
          ...rangeBetween(lbracket, rbracket),
          tag: 'List',
          contents: exprs
        })
        },
    {"name": "tuple", "symbols": [{"literal":"{"}, "_", "expr", "_", {"literal":","}, "_", "expr", "_", {"literal":"}"}], "postprocess":  
        ([lbrace, , e1, , , , e2, , rbrace]): ITuple => ({
          ...rangeBetween(lbrace, rbrace),
          tag: 'Tuple',
          contents: [e1, e2]
        })
        },
    {"name": "vector", "symbols": [{"literal":"("}, "_", "expr", "_", {"literal":","}, "expr_list", {"literal":")"}], "postprocess":  
        ([lparen, , first, , , rest, rparen]): IVector => ({
          ...rangeBetween(lparen, rparen),
          tag: 'Vector',
          contents: [first, ...rest]
        })
        },
    {"name": "bool_lit$subexpression$1", "symbols": [{"literal":"true"}]},
    {"name": "bool_lit$subexpression$1", "symbols": [{"literal":"false"}]},
    {"name": "bool_lit", "symbols": ["bool_lit$subexpression$1"], "postprocess": 
        ([[d]]): IBoolLit => ({
          ...rangeOf(d),
          tag: 'BoolLit',
          contents: d.text === 'true' // https://stackoverflow.com/questions/263965/how-can-i-convert-a-string-to-boolean-in-javascript
        })
        },
    {"name": "string_lit", "symbols": [(lexer.has("string_literal") ? {type: "string_literal"} : string_literal)], "postprocess": 
        ([d]): IStringLit => ({
          ...rangeOf(d),
          tag: 'StringLit',
          contents: JSON.parse(d.text)
        })
        },
    {"name": "annotated_float", "symbols": [{"literal":"?"}], "postprocess": ([d]): IVary => ({ ...rangeOf(d), tag: 'Vary' })},
    {"name": "annotated_float", "symbols": [(lexer.has("float_literal") ? {type: "float_literal"} : float_literal)], "postprocess":  
        ([d]): IFix => ({ ...rangeOf(d), tag: 'Fix', contents: parseFloat(d) }) 
          },
    {"name": "layering$ebnf$1", "symbols": ["layer_keyword"], "postprocess": id},
    {"name": "layering$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "layering", "symbols": ["layering$ebnf$1", "path", "__", {"literal":"below"}, "__", "path"], "postprocess": d => layering(d[0], d[1], d[5])},
    {"name": "layering$ebnf$2", "symbols": ["layer_keyword"], "postprocess": id},
    {"name": "layering$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "layering", "symbols": ["layering$ebnf$2", "path", "__", {"literal":"above"}, "__", "path"], "postprocess": d => layering(d[0], d[5], d[1])},
    {"name": "layer_keyword", "symbols": [{"literal":"layer"}, "__"], "postprocess": nth(0)},
    {"name": "computation_function", "symbols": ["identifier", "_", {"literal":"("}, "expr_list", {"literal":")"}], "postprocess":  
        ([name, , , args, rparen]): ICompApp => ({
          ...rangeBetween(name, rparen),
          tag: "CompApp",
          name, args
        }) 
        },
    {"name": "objective", "symbols": [{"literal":"encourage"}, "__", "identifier", "_", {"literal":"("}, "expr_list", {"literal":")"}], "postprocess":  
        ([kw, , name, , , args, rparen]): IObjFn => ({
          ...rangeBetween(kw, rparen),
          tag: "ObjFn",
          name, args
        }) 
        },
    {"name": "constraint", "symbols": [{"literal":"ensure"}, "__", "identifier", "_", {"literal":"("}, "expr_list", {"literal":")"}], "postprocess":  
        ([kw, , name, , , args, rparen]): IConstrFn => ({
          ...rangeBetween(kw, rparen),
          tag: "ConstrFn",
          name, args
        }) 
        },
    {"name": "expr_list", "symbols": ["_"], "postprocess": d => []},
    {"name": "expr_list$macrocall$2", "symbols": ["expr"]},
    {"name": "expr_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "expr_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "expr_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "expr_list$macrocall$3", "_", "expr_list$macrocall$2"]},
    {"name": "expr_list$macrocall$1$ebnf$1", "symbols": ["expr_list$macrocall$1$ebnf$1", "expr_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "expr_list$macrocall$1$ebnf$2", "symbols": ["expr_list$macrocall$3"], "postprocess": id},
    {"name": "expr_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "expr_list$macrocall$1", "symbols": ["expr_list$macrocall$2", "expr_list$macrocall$1$ebnf$1", "expr_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "expr_list", "symbols": ["_", "expr_list$macrocall$1", "_"], "postprocess": nth(1)},
    {"name": "gpi_decl", "symbols": ["identifier", "_", {"literal":"{"}, "property_decl_list", {"literal":"}"}], "postprocess": 
        ([shapeName, , , properties, rbrace]): GPIDecl => ({
          ...rangeBetween(shapeName, rbrace),
          tag: "GPIDecl",
          shapeName, properties
        })
        },
    {"name": "property_decl_list", "symbols": ["_"], "postprocess": () => []},
    {"name": "property_decl_list", "symbols": ["_c_", {"literal":"\n"}, "property_decl_list"], "postprocess": nth(2)},
    {"name": "property_decl_list", "symbols": ["_", "property_decl", "_"], "postprocess": d => [d[1]]},
    {"name": "property_decl_list", "symbols": ["_", "property_decl", "_c_", {"literal":"\n"}, "property_decl_list"], "postprocess": d => [d[1], ...d[4]]},
    {"name": "property_decl", "symbols": ["identifier", "_", {"literal":":"}, "_", "expr"], "postprocess": 
        ([name, , , , value]) => ({
          ...rangeBetween(name, value),
          tag: "PropertyDecl",
          name, value
        })
        },
    {"name": "identifier", "symbols": [(lexer.has("identifier") ? {type: "identifier"} : identifier)], "postprocess":  
        ([d]) => ({
          ...rangeOf(d),
          tag: 'Identifier',
          value: d.text,
          type: styleTypes.includes(d.text) ? "type-keyword" : "identifier"
        })
        },
    {"name": "comment", "symbols": [(lexer.has("comment") ? {type: "comment"} : comment)], "postprocess": convertTokenId},
    {"name": "comment", "symbols": [(lexer.has("multiline_comment") ? {type: "multiline_comment"} : multiline_comment)], "postprocess": ([d]) => rangeOf(d)},
    {"name": "_c_$ebnf$1", "symbols": []},
    {"name": "_c_$ebnf$1$subexpression$1", "symbols": [(lexer.has("ws") ? {type: "ws"} : ws)]},
    {"name": "_c_$ebnf$1$subexpression$1", "symbols": ["comment"]},
    {"name": "_c_$ebnf$1", "symbols": ["_c_$ebnf$1", "_c_$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "_c_", "symbols": ["_c_$ebnf$1"]},
    {"name": "_ml$ebnf$1", "symbols": []},
    {"name": "_ml$ebnf$1", "symbols": ["_ml$ebnf$1", "multi_line_ws_char"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "_ml", "symbols": ["_ml$ebnf$1"]},
    {"name": "multi_line_ws_char", "symbols": [(lexer.has("ws") ? {type: "ws"} : ws)]},
    {"name": "multi_line_ws_char", "symbols": [{"literal":"\n"}]},
    {"name": "multi_line_ws_char", "symbols": ["comment"]},
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
