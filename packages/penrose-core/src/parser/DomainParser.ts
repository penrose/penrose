// Generated automatically by nearley, version 2.20.1
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d: any[]): any {
  return d[0];
}
declare var string_literal: any;
declare var identifier: any;
declare var comment: any;
declare var multiline_comment: any;
declare var ws: any;

/* eslint-disable */
import * as moo from "moo";
import { concat, compact, flatten, last } from "lodash";
import {
  optional,
  tokensIn,
  basicSymbols,
  rangeOf,
  rangeBetween,
  rangeFrom,
  nth,
  convertTokenId,
} from "parser/ParserUtil";

// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  ...basicSymbols,
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
      prop: "Prop",
    }),
  },
});

const nodeData = (children: ASTNode[]) => ({
  nodeType: "Domain",
  children,
});

interface NearleyToken {
  value: any;
  [key: string]: any;
}

interface NearleyLexer {
  reset: (chunk: string, info: any) => void;
  next: () => NearleyToken | undefined;
  save: () => any;
  formatError: (token: never) => string;
  has: (tokenType: string) => boolean;
}

interface NearleyRule {
  name: string;
  symbols: NearleySymbol[];
  postprocess?: (d: any[], loc?: number, reject?: {}) => any;
}

type NearleySymbol =
  | string
  | { literal: any }
  | { test: (token: any) => boolean };

interface Grammar {
  Lexer: NearleyLexer | undefined;
  ParserRules: NearleyRule[];
  ParserStart: string;
}

const grammar: Grammar = {
  Lexer: lexer,
  ParserRules: [
    {
      name: "input",
      symbols: ["statements"],
      postprocess: ([statements]): DomainProg => ({
        ...nodeData(statements),
        ...rangeFrom(statements),
        tag: "DomainProg",
        statements,
      }),
    },
    { name: "statements", symbols: ["_"], postprocess: () => [] },
    {
      name: "statements",
      symbols: ["_c_", { literal: "\n" }, "statements"],
      postprocess: nth(2),
    },
    {
      name: "statements",
      symbols: ["_", "statement", "_c_"],
      postprocess: (d) => [d[1]],
    },
    {
      name: "statements",
      symbols: ["_", "statement", "_c_", { literal: "\n" }, "statements"],
      postprocess: (d) => [d[1], ...d[4]],
    },
    { name: "statement", symbols: ["type"], postprocess: id },
    { name: "statement", symbols: ["predicate"], postprocess: id },
    { name: "statement", symbols: ["function"], postprocess: id },
    { name: "statement", symbols: ["constructor_decl"], postprocess: id },
    { name: "statement", symbols: ["prelude"], postprocess: id },
    { name: "statement", symbols: ["notation"], postprocess: id },
    { name: "statement", symbols: ["subtype"], postprocess: id },
    {
      name: "type$ebnf$1$subexpression$1",
      symbols: [
        "_",
        { literal: "(" },
        "_",
        "type_params",
        "_",
        { literal: ")" },
      ],
    },
    {
      name: "type$ebnf$1",
      symbols: ["type$ebnf$1$subexpression$1"],
      postprocess: id,
    },
    { name: "type$ebnf$1", symbols: [], postprocess: () => null },
    {
      name: "type",
      symbols: [{ literal: "type" }, "__", "identifier", "type$ebnf$1"],
      postprocess: ([typ, , name, ps]): TypeDecl => {
        const params = ps ? ps[3] : [];
        return {
          ...nodeData([name, ...params]),
          ...rangeBetween(typ, name),
          tag: "TypeDecl",
          name,
          params,
        };
      },
    },
    {
      name: "predicate",
      symbols: [
        { literal: "predicate" },
        "__",
        "identifier",
        "type_params_list",
        "args_list",
      ],
      postprocess: ([kw, , name, params, args]): PredicateDecl => ({
        ...nodeData([name, ...params, ...args]),
        ...rangeFrom([rangeOf(kw), ...args, ...params]),
        tag: "PredicateDecl",
        name,
        params,
        args,
      }),
    },
    {
      name: "function",
      symbols: [
        { literal: "function" },
        "__",
        "identifier",
        "type_params_list",
        "args_list",
        "_",
        { literal: "->" },
        "_",
        "arg",
      ],
      postprocess: ([kw, , name, ps, as, , , , output]): FunctionDecl => {
        const params = optional(ps, []);
        const args = optional(as, []);
        return {
          ...nodeData([name, output, ...params, ...args]),
          ...rangeBetween(rangeOf(kw), output),
          tag: "FunctionDecl",
          name,
          output,
          params,
          args,
        };
      },
    },
    {
      name: "constructor_decl",
      symbols: [
        { literal: "constructor" },
        "__",
        "identifier",
        "type_params_list",
        "named_args_list",
        "_",
        { literal: "->" },
        "_",
        "arg",
      ],
      postprocess: ([kw, , name, ps, as, , , , output]): ConstructorDecl => {
        const params = optional(ps, []);
        const args = optional(as, []);
        return {
          ...nodeData([name, output, ...params, ...args]),
          ...rangeBetween(rangeOf(kw), output),
          tag: "ConstructorDecl",
          name,
          output,
          params,
          args,
        };
      },
    },
    {
      name: "prelude",
      symbols: [
        { literal: "value" },
        "__",
        "var",
        "_",
        { literal: ":" },
        "_",
        "type",
      ],
      postprocess: ([kw, , name, , , , type]): PreludeDecl => ({
        ...nodeData([name, type]),
        ...rangeBetween(rangeOf(kw), type),
        tag: "PreludeDecl",
        name,
        type,
      }),
    },
    {
      name: "notation",
      symbols: [
        { literal: "notation" },
        "_",
        "string_lit",
        "_",
        { literal: "~" },
        "_",
        "string_lit",
      ],
      postprocess: ([kw, , from, , , , to]): NotationDecl => ({
        ...nodeData([from, to]),
        ...rangeBetween(rangeOf(kw), to),
        tag: "NotationDecl",
        from,
        to,
      }),
    },
    {
      name: "subtype",
      symbols: ["type", "_", { literal: "<:" }, "_", "type"],
      postprocess: ([subType, , , , superType]): SubTypeDecl => ({
        ...nodeData([subType, superType]),
        ...rangeBetween(subType, superType),
        tag: "SubTypeDecl",
        subType,
        superType,
      }),
    },
    { name: "var", symbols: ["identifier"], postprocess: id },
    {
      name: "type_var",
      symbols: [{ literal: "'" }, "identifier"],
      postprocess: ([a, name]) => ({
        ...nodeData([name]),
        ...rangeBetween(a, name),
        tag: "TypeVar",
        name,
      }),
    },
    { name: "type", symbols: ["type_var"], postprocess: id },
    { name: "type", symbols: ["type_constructor"], postprocess: id },
    { name: "type", symbols: ["prop"], postprocess: id },
    {
      name: "type_constructor$ebnf$1",
      symbols: ["type_arg_list"],
      postprocess: id,
    },
    { name: "type_constructor$ebnf$1", symbols: [], postprocess: () => null },
    {
      name: "type_constructor",
      symbols: ["identifier", "type_constructor$ebnf$1"],
      postprocess: ([name, a]): TypeConstructor => {
        const args = optional(a, []);
        return {
          ...nodeData([name, ...args]),
          ...rangeFrom([name, ...args]),
          tag: "TypeConstructor",
          name,
          args,
        };
      },
    },
    { name: "type_arg_list$macrocall$2", symbols: ["type"] },
    { name: "type_arg_list$macrocall$3", symbols: [{ literal: "," }] },
    { name: "type_arg_list$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "type_arg_list$macrocall$1$ebnf$1$subexpression$1",
      symbols: [
        "_",
        "type_arg_list$macrocall$3",
        "_",
        "type_arg_list$macrocall$2",
      ],
    },
    {
      name: "type_arg_list$macrocall$1$ebnf$1",
      symbols: [
        "type_arg_list$macrocall$1$ebnf$1",
        "type_arg_list$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "type_arg_list$macrocall$1$ebnf$2",
      symbols: ["type_arg_list$macrocall$3"],
      postprocess: id,
    },
    {
      name: "type_arg_list$macrocall$1$ebnf$2",
      symbols: [],
      postprocess: () => null,
    },
    {
      name: "type_arg_list$macrocall$1",
      symbols: [
        "type_arg_list$macrocall$2",
        "type_arg_list$macrocall$1$ebnf$1",
        "type_arg_list$macrocall$1$ebnf$2",
      ],
      postprocess: (d) => {
        const [first, rest] = [d[0], d[1]];
        if (rest.length > 0) {
          const restNodes = rest.map((ts: any[]) => ts[3]);
          return concat(first, ...restNodes);
        } else return first;
      },
    },
    {
      name: "type_arg_list",
      symbols: [
        "_",
        { literal: "(" },
        "_",
        "type_arg_list$macrocall$1",
        "_",
        { literal: ")" },
      ],
      postprocess: ([, , , d]): Type[] => flatten(d),
    },
    { name: "type_params_list", symbols: [], postprocess: (d) => [] },
    {
      name: "type_params_list",
      symbols: [
        "_",
        { literal: "[" },
        "_",
        "type_params",
        "_",
        { literal: "]" },
      ],
      postprocess: nth(3),
    },
    { name: "type_params$macrocall$2", symbols: ["type_var"] },
    { name: "type_params$macrocall$3", symbols: [{ literal: "," }] },
    { name: "type_params$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "type_params$macrocall$1$ebnf$1$subexpression$1",
      symbols: ["_", "type_params$macrocall$3", "_", "type_params$macrocall$2"],
    },
    {
      name: "type_params$macrocall$1$ebnf$1",
      symbols: [
        "type_params$macrocall$1$ebnf$1",
        "type_params$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "type_params$macrocall$1$ebnf$2",
      symbols: ["type_params$macrocall$3"],
      postprocess: id,
    },
    {
      name: "type_params$macrocall$1$ebnf$2",
      symbols: [],
      postprocess: () => null,
    },
    {
      name: "type_params$macrocall$1",
      symbols: [
        "type_params$macrocall$2",
        "type_params$macrocall$1$ebnf$1",
        "type_params$macrocall$1$ebnf$2",
      ],
      postprocess: (d) => {
        const [first, rest] = [d[0], d[1]];
        if (rest.length > 0) {
          const restNodes = rest.map((ts: any[]) => ts[3]);
          return concat(first, ...restNodes);
        } else return first;
      },
    },
    {
      name: "type_params",
      symbols: ["type_params$macrocall$1"],
      postprocess: ([d]) => d,
    },
    { name: "args_list", symbols: [], postprocess: (d) => [] },
    { name: "args_list$macrocall$2", symbols: ["arg"] },
    { name: "args_list$macrocall$3", symbols: [{ literal: "*" }] },
    { name: "args_list$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "args_list$macrocall$1$ebnf$1$subexpression$1",
      symbols: ["_", "args_list$macrocall$3", "_", "args_list$macrocall$2"],
    },
    {
      name: "args_list$macrocall$1$ebnf$1",
      symbols: [
        "args_list$macrocall$1$ebnf$1",
        "args_list$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "args_list$macrocall$1$ebnf$2",
      symbols: ["args_list$macrocall$3"],
      postprocess: id,
    },
    {
      name: "args_list$macrocall$1$ebnf$2",
      symbols: [],
      postprocess: () => null,
    },
    {
      name: "args_list$macrocall$1",
      symbols: [
        "args_list$macrocall$2",
        "args_list$macrocall$1$ebnf$1",
        "args_list$macrocall$1$ebnf$2",
      ],
      postprocess: (d) => {
        const [first, rest] = [d[0], d[1]];
        if (rest.length > 0) {
          const restNodes = rest.map((ts: any[]) => ts[3]);
          return concat(first, ...restNodes);
        } else return first;
      },
    },
    {
      name: "args_list",
      symbols: ["_", { literal: ":" }, "_", "args_list$macrocall$1"],
      postprocess: ([, , , d]): Arg[] => flatten(d),
    },
    { name: "arg$ebnf$1$subexpression$1", symbols: ["__", "var"] },
    {
      name: "arg$ebnf$1",
      symbols: ["arg$ebnf$1$subexpression$1"],
      postprocess: id,
    },
    { name: "arg$ebnf$1", symbols: [], postprocess: () => null },
    {
      name: "arg",
      symbols: ["type", "arg$ebnf$1"],
      postprocess: ([type, v]): Arg => {
        const variable = v ? v[1] : undefined;
        const range = variable ? rangeBetween(variable, type) : rangeOf(type);
        return {
          ...nodeData(variable ? [variable, type] : [type]),
          ...range,
          tag: "Arg",
          variable,
          type,
        };
      },
    },
    { name: "named_args_list", symbols: [], postprocess: (d) => [] },
    { name: "named_args_list$macrocall$2", symbols: ["named_arg"] },
    { name: "named_args_list$macrocall$3", symbols: [{ literal: "*" }] },
    { name: "named_args_list$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "named_args_list$macrocall$1$ebnf$1$subexpression$1",
      symbols: [
        "_",
        "named_args_list$macrocall$3",
        "_",
        "named_args_list$macrocall$2",
      ],
    },
    {
      name: "named_args_list$macrocall$1$ebnf$1",
      symbols: [
        "named_args_list$macrocall$1$ebnf$1",
        "named_args_list$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "named_args_list$macrocall$1$ebnf$2",
      symbols: ["named_args_list$macrocall$3"],
      postprocess: id,
    },
    {
      name: "named_args_list$macrocall$1$ebnf$2",
      symbols: [],
      postprocess: () => null,
    },
    {
      name: "named_args_list$macrocall$1",
      symbols: [
        "named_args_list$macrocall$2",
        "named_args_list$macrocall$1$ebnf$1",
        "named_args_list$macrocall$1$ebnf$2",
      ],
      postprocess: (d) => {
        const [first, rest] = [d[0], d[1]];
        if (rest.length > 0) {
          const restNodes = rest.map((ts: any[]) => ts[3]);
          return concat(first, ...restNodes);
        } else return first;
      },
    },
    {
      name: "named_args_list",
      symbols: ["_", { literal: ":" }, "_", "named_args_list$macrocall$1"],
      postprocess: ([, , , d]): Arg[] => flatten(d),
    },
    {
      name: "named_arg",
      symbols: ["type", "__", "var"],
      postprocess: ([type, , variable]): Arg => ({
        ...nodeData([type, variable]),
        ...rangeBetween(type, variable),
        tag: "Arg",
        variable,
        type,
      }),
    },
    {
      name: "prop",
      symbols: [{ literal: "Prop" }],
      postprocess: ([kw]): Prop => ({
        ...nodeData([]),
        ...rangeOf(kw),
        tag: "Prop",
      }),
    },
    {
      name: "string_lit",
      symbols: [
        lexer.has("string_literal")
          ? { type: "string_literal" }
          : string_literal,
      ],
      postprocess: ([d]): IStringLit => ({
        ...nodeData([]),
        ...rangeOf(d),
        tag: "StringLit",
        contents: d.value,
      }),
    },
    {
      name: "identifier",
      symbols: [lexer.has("identifier") ? { type: "identifier" } : identifier],
      postprocess: ([d]) => ({
        ...nodeData([]),
        ...rangeOf(d),
        tag: "Identifier",
        value: d.text,
        type: "identifier",
      }),
    },
    {
      name: "comment",
      symbols: [lexer.has("comment") ? { type: "comment" } : comment],
      postprocess: convertTokenId,
    },
    {
      name: "comment",
      symbols: [
        lexer.has("multiline_comment")
          ? { type: "multiline_comment" }
          : multiline_comment,
      ],
      postprocess: ([d]) => rangeOf(d),
    },
    { name: "_c_$ebnf$1", symbols: [] },
    {
      name: "_c_$ebnf$1$subexpression$1",
      symbols: [lexer.has("ws") ? { type: "ws" } : ws],
    },
    { name: "_c_$ebnf$1$subexpression$1", symbols: ["comment"] },
    {
      name: "_c_$ebnf$1",
      symbols: ["_c_$ebnf$1", "_c_$ebnf$1$subexpression$1"],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    { name: "_c_", symbols: ["_c_$ebnf$1"] },
    { name: "_ml$ebnf$1", symbols: [] },
    {
      name: "_ml$ebnf$1",
      symbols: ["_ml$ebnf$1", "multi_line_ws_char"],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    { name: "_ml", symbols: ["_ml$ebnf$1"] },
    {
      name: "multi_line_ws_char",
      symbols: [lexer.has("ws") ? { type: "ws" } : ws],
    },
    { name: "multi_line_ws_char", symbols: [{ literal: "\n" }] },
    { name: "multi_line_ws_char", symbols: ["comment"] },
    { name: "__$ebnf$1", symbols: [lexer.has("ws") ? { type: "ws" } : ws] },
    {
      name: "__$ebnf$1",
      symbols: ["__$ebnf$1", lexer.has("ws") ? { type: "ws" } : ws],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    { name: "__", symbols: ["__$ebnf$1"] },
    { name: "_$ebnf$1", symbols: [] },
    {
      name: "_$ebnf$1",
      symbols: ["_$ebnf$1", lexer.has("ws") ? { type: "ws" } : ws],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    { name: "_", symbols: ["_$ebnf$1"] },
  ],
  ParserStart: "input",
};

export default grammar;
