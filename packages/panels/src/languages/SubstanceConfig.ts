import { Env } from "@penrose/core";
import { IRange, languages } from "monaco-editor";
import { CommentCommon, CommonTokens } from "./common";

export const SubstanceConfig: languages.LanguageConfiguration = {
  comments: {
    blockComment: ["/*", "*/"],
    lineComment: "--",
  },
  autoClosingPairs: [
    { open: "(", close: ")", notIn: ["string", "comment"] },
    { open: '"', close: '"', notIn: ["string", "comment"] },
    { open: "$", close: "$", notIn: ["string", "comment"] },
  ],
  surroundingPairs: [
    { open: "(", close: ")" },
    { open: '"', close: '"' },
    { open: "$", close: "$" },
  ],
  brackets: [["(", ")"]],
  folding: {
    markers: {
      start: /\(/,
      end: /\)/,
    },
  },
};

export const SubstanceLanguageTokens = (
  domainCache: Env
): languages.IMonarchLanguage => {
  const refs = {
    types: Array.from(domainCache.types.keys()),
    functionLikes: [
      ...Array.from(domainCache.constructors.keys()),
      ...Array.from(domainCache.functions.keys()),
      ...Array.from(domainCache.predicates.keys()),
    ],
    control: ["AutoLabel", "Label", "NoLabel", "All"],
  };
  return {
    ...refs,
    tokenizer: {
      root: [
        ...CommonTokens,
        [/[()]/, "@brackets"],
        [/\$.*\$/, "comment.doc"],
        [
          /[a-z_A-Z$][\w$]*/,
          {
            cases: {
              "@types": "keyword",
              "@control": "keyword",
              "@functionLikes": "tag",
              "@default": "identifier",
            },
          },
        ],
        { include: "@whitespace" },
      ],
      ...CommentCommon,
    },
  };
};

export const SubstanceCompletions = (
  range: IRange,
  domainCache: any
): languages.CompletionItem[] => {
  const types = Array.from(domainCache.types.keys()).map((type) => ({
    label: type,
    insertText: type + " $0",
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.TypeParameter,
    detail: "type",
    range,
  }));
  const predicates = Array.from(domainCache.predicates.keys()).map((type) => ({
    label: type,
    insertText: type + "($0)",
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.Property,
    detail: "predicate",
    range,
  }));
  const constructors = Array.from(domainCache.constructors.keys()).map(
    (type) => ({
      label: type,
      insertText: type + "($0)",
      insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
      kind: languages.CompletionItemKind.Constructor,
      detail: "constructor",
      range,
    })
  );
  const labeling = ["AutoLabel", "Label", "NoLabel", "All"].map(
    (type: any) => ({
      label: type,
      insertText: type,
      kind: languages.CompletionItemKind.Color,
      detail: "labeling",
      range,
    })
  );

  const fns = Array.from(domainCache.functions.entries()).map(
    ([name, fn]: any) => ({
      label: name,
      insertText: `${name}($0)`,
      insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
      kind: languages.CompletionItemKind.Function,
      detail: `function -> ${fn.output.type.name.value}`,
      documentation: {
        value: `${fn.args
          .map((arg: any) => {
            return arg.type.name.value + " " + arg.variable.value;
          })
          .join(" * ")} -> ${fn.output.type.name.value}`,
      },
      range,
    })
  );

  return [...types, ...fns, ...predicates, ...constructors, ...labeling];
};
