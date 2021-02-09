// Generated automatically by nearley, version 2.20.1
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d: any[]): any {
  return d[0];
}
declare var string_literal: any;
declare var tex_literal: any;
declare var identifier: any;
declare var comment: any;
declare var multiline_comment: any;
declare var ws: any;

/* eslint-disable */
import * as moo from "moo";
import { concat, compact, flatten, last } from "lodash";
import {
  optional,
  basicSymbols,
  rangeOf,
  rangeBetween,
  rangeFrom,
  nth,
  convertTokenId,
} from "parser/ParserUtil";

// NOTE: ordering matters here. Top patterns get matched __first__
const lexer = moo.compile({
  tex_literal: /\$.*?\$/,
  double_arrow: "<->",
  ...basicSymbols,
  // tex_literal: /\$(?:[^\n\$]|\\["\\ntbfr])*\$/,
  identifier: {
    match: /[A-z_][A-Za-z_0-9]*/,
    type: moo.keywords({
      // NOTE: the next line add type annotation keywords into the keyword set and thereby forbidding users to use keywords like `shape`
      // "type-keyword": styleTypes,
      all: "All",
      label: "Label",
      noLabel: "NoLabel",
      autoLabel: "AutoLabel",
    }),
  },
});

const nodeData = (children: ASTNode[]) => ({
  nodeType: "Substance",
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
      postprocess: ([d]): SubProg => {
        const statements = flatten(d) as SubStmt[];
        return {
          ...nodeData(statements),
          ...rangeFrom(statements),
          tag: "SubProg",
          statements,
        };
      },
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
    { name: "statement", symbols: ["decl"], postprocess: id },
    { name: "statement", symbols: ["bind"], postprocess: id },
    { name: "statement", symbols: ["decl_bind"], postprocess: id },
    { name: "statement", symbols: ["apply_predicate"], postprocess: id },
    { name: "statement", symbols: ["label_stmt"], postprocess: id },
    { name: "statement", symbols: ["equal_exprs"], postprocess: id },
    { name: "statement", symbols: ["equal_predicates"], postprocess: id },
    { name: "decl$macrocall$2", symbols: ["identifier"] },
    { name: "decl$macrocall$3", symbols: [{ literal: "," }] },
    { name: "decl$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "decl$macrocall$1$ebnf$1$subexpression$1",
      symbols: ["_", "decl$macrocall$3", "_", "decl$macrocall$2"],
    },
    {
      name: "decl$macrocall$1$ebnf$1",
      symbols: [
        "decl$macrocall$1$ebnf$1",
        "decl$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "decl$macrocall$1$ebnf$2",
      symbols: ["decl$macrocall$3"],
      postprocess: id,
    },
    { name: "decl$macrocall$1$ebnf$2", symbols: [], postprocess: () => null },
    {
      name: "decl$macrocall$1",
      symbols: [
        "decl$macrocall$2",
        "decl$macrocall$1$ebnf$1",
        "decl$macrocall$1$ebnf$2",
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
      name: "decl",
      symbols: ["type_constructor", "__", "decl$macrocall$1"],
      postprocess: ([type, , ids]): Decl[] =>
        ids.map(
          (name: Identifier): Decl => ({
            ...nodeData([type, name]),
            ...rangeBetween(type, name),
            tag: "Decl",
            type,
            name,
          })
        ),
    },
    {
      name: "bind",
      symbols: ["identifier", "_", { literal: ":=" }, "_", "sub_expr"],
      postprocess: ([variable, , , , expr]): Bind => ({
        ...nodeData([variable, expr]),
        ...rangeBetween(variable, expr),
        tag: "Bind",
        variable,
        expr,
      }),
    },
    {
      name: "decl_bind",
      symbols: [
        "type_constructor",
        "__",
        "identifier",
        "_",
        { literal: ":=" },
        "_",
        "sub_expr",
      ],
      postprocess: ([type, , variable, , , , expr]): [Decl, Bind] => {
        const decl: Decl = {
          ...nodeData([type, variable]),
          ...rangeBetween(type, variable),
          tag: "Decl",
          type,
          name: variable,
        };
        const bind: Bind = {
          ...nodeData([variable, expr]),
          ...rangeBetween(variable, expr),
          tag: "Bind",
          variable,
          expr,
        };
        return [decl, bind];
      },
    },
    { name: "apply_predicate$macrocall$2", symbols: ["pred_arg"] },
    { name: "apply_predicate$macrocall$3", symbols: [{ literal: "," }] },
    { name: "apply_predicate$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "apply_predicate$macrocall$1$ebnf$1$subexpression$1",
      symbols: [
        "_",
        "apply_predicate$macrocall$3",
        "_",
        "apply_predicate$macrocall$2",
      ],
    },
    {
      name: "apply_predicate$macrocall$1$ebnf$1",
      symbols: [
        "apply_predicate$macrocall$1$ebnf$1",
        "apply_predicate$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "apply_predicate$macrocall$1$ebnf$2",
      symbols: ["apply_predicate$macrocall$3"],
      postprocess: id,
    },
    {
      name: "apply_predicate$macrocall$1$ebnf$2",
      symbols: [],
      postprocess: () => null,
    },
    {
      name: "apply_predicate$macrocall$1",
      symbols: [
        "apply_predicate$macrocall$2",
        "apply_predicate$macrocall$1$ebnf$1",
        "apply_predicate$macrocall$1$ebnf$2",
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
      name: "apply_predicate",
      symbols: [
        "identifier",
        "_",
        { literal: "(" },
        "_",
        "apply_predicate$macrocall$1",
        "_",
        { literal: ")" },
      ],
      postprocess: ([name, , , , args]): ApplyPredicate => ({
        ...nodeData([name, ...args]),
        ...rangeFrom([name, ...args]),
        tag: "ApplyPredicate",
        name,
        args,
      }),
    },
    { name: "pred_arg", symbols: ["sub_expr"], postprocess: id },
    { name: "sub_expr", symbols: ["identifier"], postprocess: id },
    { name: "sub_expr", symbols: ["deconstructor"], postprocess: id },
    { name: "sub_expr", symbols: ["func"], postprocess: id },
    { name: "sub_expr", symbols: ["string_lit"], postprocess: id },
    {
      name: "deconstructor",
      symbols: ["identifier", "_", { literal: "." }, "_", "identifier"],
      postprocess: ([variable, , , , field]): Deconstructor => ({
        ...nodeData([variable, field]),
        ...rangeBetween(variable, field),
        tag: "Deconstructor",
        variable,
        field,
      }),
    },
    { name: "func$macrocall$2", symbols: ["sub_expr"] },
    { name: "func$macrocall$3", symbols: [{ literal: "," }] },
    {
      name: "func$macrocall$1$ebnf$1",
      symbols: ["func$macrocall$2"],
      postprocess: id,
    },
    { name: "func$macrocall$1$ebnf$1", symbols: [], postprocess: () => null },
    { name: "func$macrocall$1$ebnf$2", symbols: [] },
    {
      name: "func$macrocall$1$ebnf$2$subexpression$1",
      symbols: ["_", "func$macrocall$3", "_", "func$macrocall$2"],
    },
    {
      name: "func$macrocall$1$ebnf$2",
      symbols: [
        "func$macrocall$1$ebnf$2",
        "func$macrocall$1$ebnf$2$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "func$macrocall$1",
      symbols: ["func$macrocall$1$ebnf$1", "func$macrocall$1$ebnf$2"],
      postprocess: (d) => {
        const [first, rest] = [d[0], d[1]];
        if (!first) return [];
        if (rest.length > 0) {
          const restNodes = rest.map((ts: any[]) => ts[3]);
          return concat(first, ...restNodes);
        } else return first;
      },
    },
    {
      name: "func",
      symbols: [
        "identifier",
        "_",
        { literal: "(" },
        "_",
        "func$macrocall$1",
        "_",
        { literal: ")" },
      ],
      postprocess: ([name, , , , args]): Func => ({
        ...nodeData([name, ...args]),
        ...rangeFrom([name, ...args]),
        tag: "Func",
        name,
        args,
      }),
    },
    {
      name: "equal_exprs",
      symbols: ["sub_expr", "_", { literal: "=" }, "_", "sub_expr"],
      postprocess: ([left, , , , right]): EqualExprs => ({
        ...nodeData([left, right]),
        ...rangeBetween(left, right),
        tag: "EqualExprs",
        left,
        right,
      }),
    },
    {
      name: "equal_predicates",
      symbols: [
        "apply_predicate",
        "_",
        { literal: "<->" },
        "_",
        "apply_predicate",
      ],
      postprocess: ([left, , , , right]): EqualPredicates => ({
        ...nodeData([left, right]),
        ...rangeBetween(left, right),
        tag: "EqualPredicates",
        left,
        right,
      }),
    },
    { name: "label_stmt", symbols: ["label_decl"], postprocess: id },
    { name: "label_stmt", symbols: ["no_label"], postprocess: id },
    { name: "label_stmt", symbols: ["auto_label"], postprocess: id },
    {
      name: "label_decl",
      symbols: [{ literal: "Label" }, "__", "identifier", "__", "tex_literal"],
      postprocess: ([kw, , variable, , label]): LabelDecl => ({
        ...nodeData([variable, label]),
        ...rangeBetween(rangeOf(kw), label),
        tag: "LabelDecl",
        variable,
        label,
      }),
    },
    { name: "no_label$macrocall$2", symbols: ["identifier"] },
    { name: "no_label$macrocall$3", symbols: [{ literal: "," }] },
    { name: "no_label$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "no_label$macrocall$1$ebnf$1$subexpression$1",
      symbols: ["_", "no_label$macrocall$3", "_", "no_label$macrocall$2"],
    },
    {
      name: "no_label$macrocall$1$ebnf$1",
      symbols: [
        "no_label$macrocall$1$ebnf$1",
        "no_label$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "no_label$macrocall$1$ebnf$2",
      symbols: ["no_label$macrocall$3"],
      postprocess: id,
    },
    {
      name: "no_label$macrocall$1$ebnf$2",
      symbols: [],
      postprocess: () => null,
    },
    {
      name: "no_label$macrocall$1",
      symbols: [
        "no_label$macrocall$2",
        "no_label$macrocall$1$ebnf$1",
        "no_label$macrocall$1$ebnf$2",
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
      name: "no_label",
      symbols: [{ literal: "NoLabel" }, "__", "no_label$macrocall$1"],
      postprocess: ([kw, , args]): NoLabel => ({
        ...nodeData([...args]),
        ...rangeFrom([rangeOf(kw), ...args]),
        tag: "NoLabel",
        args,
      }),
    },
    {
      name: "auto_label",
      symbols: [{ literal: "AutoLabel" }, "__", "label_option"],
      postprocess: ([kw, , option]): AutoLabel => ({
        ...nodeData([option]),
        ...rangeBetween(kw, option),
        tag: "AutoLabel",
        option,
      }),
    },
    {
      name: "label_option",
      symbols: [{ literal: "All" }],
      postprocess: ([kw]): LabelOption => ({
        ...nodeData([]),
        ...rangeOf(kw),
        tag: "DefaultLabels",
      }),
    },
    { name: "label_option$macrocall$2", symbols: ["identifier"] },
    { name: "label_option$macrocall$3", symbols: [{ literal: "," }] },
    { name: "label_option$macrocall$1$ebnf$1", symbols: [] },
    {
      name: "label_option$macrocall$1$ebnf$1$subexpression$1",
      symbols: [
        "_",
        "label_option$macrocall$3",
        "_",
        "label_option$macrocall$2",
      ],
    },
    {
      name: "label_option$macrocall$1$ebnf$1",
      symbols: [
        "label_option$macrocall$1$ebnf$1",
        "label_option$macrocall$1$ebnf$1$subexpression$1",
      ],
      postprocess: (d) => d[0].concat([d[1]]),
    },
    {
      name: "label_option$macrocall$1$ebnf$2",
      symbols: ["label_option$macrocall$3"],
      postprocess: id,
    },
    {
      name: "label_option$macrocall$1$ebnf$2",
      symbols: [],
      postprocess: () => null,
    },
    {
      name: "label_option$macrocall$1",
      symbols: [
        "label_option$macrocall$2",
        "label_option$macrocall$1$ebnf$1",
        "label_option$macrocall$1$ebnf$2",
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
      name: "label_option",
      symbols: ["label_option$macrocall$1"],
      postprocess: ([variables]): LabelOption => ({
        ...nodeData([]),
        ...rangeFrom(variables),
        tag: "LabelIDs",
        variables,
      }),
    },
    {
      name: "type_constructor$ebnf$1",
      symbols: ["type_arg_list"],
      postprocess: id,
    },
    { name: "type_constructor$ebnf$1", symbols: [], postprocess: () => null },
    {
      name: "type_constructor",
      symbols: ["identifier", "type_constructor$ebnf$1"],
      postprocess: ([name, a]): TypeConsApp => {
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
    { name: "type_arg_list$macrocall$2", symbols: ["type_constructor"] },
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
      postprocess: ([, , , d]): TypeConsApp[] => flatten(d),
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
      name: "tex_literal",
      symbols: [
        lexer.has("tex_literal") ? { type: "tex_literal" } : tex_literal,
      ],
      postprocess: ([d]): IStringLit => ({
        ...nodeData([]),
        ...rangeOf(d),
        tag: "StringLit",
        contents: d.text.substring(1, d.text.length - 1), // NOTE: remove dollars
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
