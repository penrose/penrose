import { Monaco } from "@monaco-editor/react";
import { IRange, languages } from "monaco-editor";
import { CommentCommon, CommonTokens } from "./common";

export const DomainConfig: languages.LanguageConfiguration = {
  comments: {
    blockComment: ["/*", "*/"],
    lineComment: "--",
  },
  autoClosingPairs: [{ open: '"', close: '"', notIn: ["string", "comment"] }],
  surroundingPairs: [{ open: '"', close: '"' }],
};

const domainOperators: string[] = ["*", "->", ":", "<:"];

const domainKeywords: string[] = [
  "type",
  "notation",
  "predicate",
  "function",
  "constructor",
  "prelude",
];

export const DomainLanguageTokens = (): languages.IMonarchLanguage => {
  const refs = {
    control: domainKeywords,
    operator: domainOperators,
  };
  return {
    ...refs,
    tokenizer: {
      root: [
        ...CommonTokens,
        [/\$.*\$/, "comment.doc"],
        [
          /[a-z_A-Z$][\w$]*/,
          {
            cases: {
              "@control": "keyword",
              "@default": "identifier",
              "@operator": "operator",
            },
          },
        ],
        { include: "@whitespace" },
      ],
      ...CommentCommon,
    },
  };
};

export const DomainCompletions = (range: IRange) => {
  return domainKeywords.map((type) => ({
    label: type,
    insertText: type,
    kind: languages.CompletionItemKind.Keyword,
    detail: "Domain keywords",
    range,
  }));
};

export const SetupDomainMonaco = (monaco: Monaco) => {
  monaco.languages.register({ id: "domain" });
  monaco.languages.setLanguageConfiguration("domain", DomainConfig);
  monaco.languages.setMonarchTokensProvider("domain", DomainLanguageTokens());
  const dispose = monaco.languages.registerCompletionItemProvider("domain", {
    provideCompletionItems: (model, position) => {
      const word = model.getWordUntilPosition(position);
      const range: IRange = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      };
      return { suggestions: DomainCompletions(range) };
    },
  });
  return () => {
    dispose.dispose();
  };
};
