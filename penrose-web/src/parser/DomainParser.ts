// Generated automatically by nearley, version 2.20.1
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d: any[]): any { return d[0]; }
declare var identifier: any;
declare var comment: any;
declare var multiline_comment: any;
declare var ws: any;


import * as moo from "moo";
import { concat, compact, flatten, last } from 'lodash'
import { rangeOf, rangeBetween, rangeFrom, nth, convertTokenId } from 'parser/ParserUtil'

// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  ws: /[ \t]+/,
  nl: { match: "\n", lineBreaks: true },
  lte: "<=",
  lt: "<",
  gte: ">=",
  gt: ">",
  eq: "==",
  rarrow: "->",
  lparen: "(",
  rparen: ")",
  apos: "'",
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
  dollar: "$",
  tick: "\`",
  identifier: {
    match: /[A-z_][A-Za-z_0-9]*/,
    type: moo.keywords({
      // NOTE: the next line add type annotation keywords into the keyword set and thereby forbidding users to use keywords like `shape`
      // "type-keyword": styleTypes, 
      type: "type",
      value: "value",
      constructor: "constructor",
      function: "function",
      predicate: "predicate",
      notation: "notation",
      prop: "Prop"
    })
  }
});

const optional = <T>(optionalValue: T | undefined, defaultValue: T) => optionalValue ? optionalValue : defaultValue;
// Helper that takes in a mix of single token or list of tokens, drops all undefined (i.e. optional ealues), and finally flattten the mixture to a list of tokens.
const tokensIn = (tokenList: any[]): any[] => flatten(compact(tokenList));


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
    {"name": "input", "symbols": ["statements"], "postprocess":  
        ([statements]): DomainProg => ({
          ...rangeFrom(statements),
          tag: "DomainProg",
          statements
        })
        },
    {"name": "statements", "symbols": ["_"], "postprocess": () => []},
    {"name": "statements", "symbols": ["_c_", {"literal":"\n"}, "statements"], "postprocess": nth(2)},
    {"name": "statements", "symbols": ["_", "statement", "_"], "postprocess": d => [d[1]]},
    {"name": "statements", "symbols": ["_", "statement", "_c_", {"literal":"\n"}, "statements"], "postprocess": d => [d[1], ...d[4]]},
    {"name": "statement", "symbols": ["type"], "postprocess": id},
    {"name": "statement", "symbols": ["predicate"], "postprocess": id},
    {"name": "statement", "symbols": ["function"], "postprocess": id},
    {"name": "type$ebnf$1$subexpression$1", "symbols": ["_", {"literal":"("}, "_", "type_params", "_", {"literal":")"}]},
    {"name": "type$ebnf$1", "symbols": ["type$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "type$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "type", "symbols": [{"literal":"type"}, "__", "identifier", "type$ebnf$1"], "postprocess": 
        ([typ, , name, params]): TypeDecl => ({
          ...rangeBetween(typ, name),
          tag: "TypeDecl", 
          name, 
          params: params ? params[3] : []
        })
        },
    {"name": "predicate", "symbols": ["nested_predicate"], "postprocess": id},
    {"name": "predicate", "symbols": ["simple_predicate"], "postprocess": id},
    {"name": "simple_predicate$ebnf$1", "symbols": ["type_params_list"], "postprocess": id},
    {"name": "simple_predicate$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "simple_predicate$ebnf$2", "symbols": ["args_list"], "postprocess": id},
    {"name": "simple_predicate$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "simple_predicate", "symbols": [{"literal":"predicate"}, "__", "identifier", "_", {"literal":":"}, "simple_predicate$ebnf$1", "_", "simple_predicate$ebnf$2"], "postprocess": 
        ([kw, , name, , , params, , args]): PredicateDecl => ({
          // HACK: keywords don't seem to have ranges. Have to manually convert here
          ...rangeFrom(tokensIn([rangeOf(kw), args, params])),
          tag: "PredicateDecl",
          name, 
          params: optional(params, []),
          args: optional(args, []),
        })
        },
    {"name": "nested_predicate$ebnf$1", "symbols": ["prop_list"], "postprocess": id},
    {"name": "nested_predicate$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "nested_predicate", "symbols": [{"literal":"predicate"}, "__", "identifier", "_", {"literal":":"}, "_", "nested_predicate$ebnf$1"], "postprocess": 
        ([kw, , name, , , , args]): NestedPredicateDecl => ({
          // HACK: keywords don't seem to have ranges. Have to manually convert here
          ...rangeFrom(tokensIn([rangeOf(kw), args])),
          tag: "NestedPredicateDecl",
          name, args
        })
        },
    {"name": "function$ebnf$1", "symbols": ["type_params_list"], "postprocess": id},
    {"name": "function$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "function$ebnf$2", "symbols": ["named_args_list"], "postprocess": id},
    {"name": "function$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "function", "symbols": [{"literal":"function"}, "__", "identifier", "_", {"literal":":"}, "function$ebnf$1", "_", "function$ebnf$2", "_", {"literal":"->"}, "_", "arg"], "postprocess": 
        ([kw, , name, , , params, , args, , , , output]): FunctionDecl => ({
          ...rangeBetween(rangeOf(kw), output),
          tag: "FunctionDecl",
          name, output,
          params: optional(params, []),
          args: optional(args, []),
        })
          },
    {"name": "variable", "symbols": ["var"], "postprocess": id},
    {"name": "variable", "symbols": ["type_var"], "postprocess": id},
    {"name": "var", "symbols": ["identifier"], "postprocess": ([name]): VarConst => ({ ...rangeOf(name), tag: "VarConst", name })},
    {"name": "type_var", "symbols": [{"literal":"'"}, "identifier"], "postprocess":  
        ([a, name]) => ({ ...rangeBetween(a, name), tag: "TypeVar", name }) 
        },
    {"name": "kind", "symbols": ["type"], "postprocess": id},
    {"name": "kind", "symbols": [{"literal":"type"}], "postprocess": ([d]): ConstType => ({ ...rangeOf(d), tag: "ConstType", contents: "type" })},
    {"name": "type", "symbols": ["type_var"], "postprocess": id},
    {"name": "type", "symbols": ["type_constructor"], "postprocess": id},
    {"name": "type_constructor$ebnf$1", "symbols": ["type_arg_list"], "postprocess": id},
    {"name": "type_constructor$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "type_constructor", "symbols": ["identifier", "type_constructor$ebnf$1"], "postprocess":  
        ([name, args]): TypeConstructor => ({
          ...rangeBetween(name, args),
          tag: "TypeConstructor",
          name, 
          args: optional(args, [])
        })
         },
    {"name": "type_params_list", "symbols": ["_", {"literal":"["}, "_", "type_params", "_", {"literal":"]"}], "postprocess": nth(3)},
    {"name": "type_params$macrocall$2", "symbols": ["type_param"]},
    {"name": "type_params$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "type_params$macrocall$1$ebnf$1", "symbols": []},
    {"name": "type_params$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "type_params$macrocall$3", "_", "type_params$macrocall$2"]},
    {"name": "type_params$macrocall$1$ebnf$1", "symbols": ["type_params$macrocall$1$ebnf$1", "type_params$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "type_params$macrocall$1$ebnf$2", "symbols": ["type_params$macrocall$3"], "postprocess": id},
    {"name": "type_params$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "type_params$macrocall$1", "symbols": ["type_params$macrocall$2", "type_params$macrocall$1$ebnf$1", "type_params$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "type_params", "symbols": ["type_params$macrocall$1"], "postprocess": ([d]) => d},
    {"name": "type_param", "symbols": ["variable", "_", {"literal":":"}, "_", "kind"], "postprocess":  
        ([variable, , , , kind]): TypeParam => ({
          ...rangeBetween(variable, kind),
          tag: "TypeParam", variable, kind
        })
        },
    {"name": "type_arg_list$macrocall$2", "symbols": ["type_arg"]},
    {"name": "type_arg_list$macrocall$3", "symbols": [{"literal":","}]},
    {"name": "type_arg_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "type_arg_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "type_arg_list$macrocall$3", "_", "type_arg_list$macrocall$2"]},
    {"name": "type_arg_list$macrocall$1$ebnf$1", "symbols": ["type_arg_list$macrocall$1$ebnf$1", "type_arg_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "type_arg_list$macrocall$1$ebnf$2", "symbols": ["type_arg_list$macrocall$3"], "postprocess": id},
    {"name": "type_arg_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "type_arg_list$macrocall$1", "symbols": ["type_arg_list$macrocall$2", "type_arg_list$macrocall$1$ebnf$1", "type_arg_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "type_arg_list", "symbols": ["_", {"literal":"("}, "_", "type_arg_list$macrocall$1", "_", {"literal":")"}], "postprocess": ([, , , d]): TypeArg[] => flatten(d)},
    {"name": "type_arg", "symbols": ["var"], "postprocess": id},
    {"name": "type_arg", "symbols": ["type"], "postprocess": id},
    {"name": "args_list$macrocall$2", "symbols": ["arg"]},
    {"name": "args_list$macrocall$3", "symbols": [{"literal":"*"}]},
    {"name": "args_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "args_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "args_list$macrocall$3", "_", "args_list$macrocall$2"]},
    {"name": "args_list$macrocall$1$ebnf$1", "symbols": ["args_list$macrocall$1$ebnf$1", "args_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "args_list$macrocall$1$ebnf$2", "symbols": ["args_list$macrocall$3"], "postprocess": id},
    {"name": "args_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "args_list$macrocall$1", "symbols": ["args_list$macrocall$2", "args_list$macrocall$1$ebnf$1", "args_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "args_list", "symbols": ["args_list$macrocall$1"], "postprocess": ([d]): Arg[] => flatten(d)},
    {"name": "named_args_list$macrocall$2", "symbols": ["named_arg"]},
    {"name": "named_args_list$macrocall$3", "symbols": [{"literal":"*"}]},
    {"name": "named_args_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "named_args_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "named_args_list$macrocall$3", "_", "named_args_list$macrocall$2"]},
    {"name": "named_args_list$macrocall$1$ebnf$1", "symbols": ["named_args_list$macrocall$1$ebnf$1", "named_args_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "named_args_list$macrocall$1$ebnf$2", "symbols": ["named_args_list$macrocall$3"], "postprocess": id},
    {"name": "named_args_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "named_args_list$macrocall$1", "symbols": ["named_args_list$macrocall$2", "named_args_list$macrocall$1$ebnf$1", "named_args_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "named_args_list", "symbols": ["named_args_list$macrocall$1"], "postprocess": ([d]): Arg[] => flatten(d)},
    {"name": "arg$ebnf$1$subexpression$1", "symbols": ["__", "var"]},
    {"name": "arg$ebnf$1", "symbols": ["arg$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "arg$ebnf$1", "symbols": [], "postprocess": () => null},
    {"name": "arg", "symbols": ["type", "arg$ebnf$1"], "postprocess":  
        ([type, v]): Arg => {
          const variable = v ? v[1] : undefined;
          const range = variable ? rangeBetween(variable, type) : rangeOf(type);
          return { ...range, tag: "Arg", variable: variable, type };
        }
        },
    {"name": "named_arg", "symbols": ["type", "__", "var"], "postprocess":  
        ([type, , variable]): Arg => ({
           ...rangeBetween(type, variable), 
           tag: "Arg", variable: variable, type 
        })
        },
    {"name": "prop", "symbols": [{"literal":"Prop"}, "_", "var"], "postprocess": nth(2)},
    {"name": "prop_list$macrocall$2", "symbols": ["prop"]},
    {"name": "prop_list$macrocall$3", "symbols": [{"literal":"*"}]},
    {"name": "prop_list$macrocall$1$ebnf$1", "symbols": []},
    {"name": "prop_list$macrocall$1$ebnf$1$subexpression$1", "symbols": ["_", "prop_list$macrocall$3", "_", "prop_list$macrocall$2"]},
    {"name": "prop_list$macrocall$1$ebnf$1", "symbols": ["prop_list$macrocall$1$ebnf$1", "prop_list$macrocall$1$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "prop_list$macrocall$1$ebnf$2", "symbols": ["prop_list$macrocall$3"], "postprocess": id},
    {"name": "prop_list$macrocall$1$ebnf$2", "symbols": [], "postprocess": () => null},
    {"name": "prop_list$macrocall$1", "symbols": ["prop_list$macrocall$2", "prop_list$macrocall$1$ebnf$1", "prop_list$macrocall$1$ebnf$2"], "postprocess":  
        d => { 
          const [first, rest] = [d[0], d[1]];
          if(rest.length > 0) {
            const restNodes = rest.map((ts: any[]) => ts[3]);
            return concat(first, ...restNodes);
          } else return first;
        }
        },
    {"name": "prop_list", "symbols": ["prop_list$macrocall$1"], "postprocess": ([d]) => d},
    {"name": "identifier", "symbols": [(lexer.has("identifier") ? {type: "identifier"} : identifier)], "postprocess":  
        ([d]) => ({
          ...rangeOf(d),
          tag: 'Identifier',
          value: d.text,
          type: "identifier"
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
