import { Monaco } from "@monaco-editor/react";
import {
  compDict,
  constrDict,
  hexToRgba,
  objDict,
  rgbaToHex,
  shapeTypes,
} from "@penrose/core";
import { IRange, editor, languages } from "monaco-editor";
import { CommentCommon, CommonTokens } from "./common.js";

export const StyleConfig: languages.LanguageConfiguration = {
  comments: {
    blockComment: ["/*", "*/"],
    lineComment: "--",
  },
  autoClosingPairs: [
    { open: "{", close: "}", notIn: ["string", "comment"] },
    { open: "[", close: "]", notIn: ["string", "comment"] },
    { open: "(", close: ")", notIn: ["string", "comment"] },
    { open: '"', close: '"', notIn: ["string", "comment"] },
  ],
  surroundingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
    { open: '"', close: '"' },
    { open: "'", close: "'" },
  ],
  brackets: [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
  ],
  folding: {
    markers: {
      start: /\{/,
      end: /\}/,
    },
  },
};

const styleCustoms = {
  keywords: [
    "forall",
    "collect",
    "into",
    "foreach",
    "where",
    "with",
    "listof",
    "from",
    "delete",
    "as",
    "true",
    "false",
    "layer",
    "encourage",
    "ensure",
    "override",
    "above",
    "below",
    "has",
    "math",
    "text",
    "in",
    "except",
    "repeatable",
  ],
  types: [
    "scalar",
    "int",
    "bool",
    "string",
    "path",
    "color",
    "file",
    "style",
    "shape",
    "vec2",
    "vec3",
    "vec4",
    "mat2x2",
    "mat3x3",
    "mat4x4",
    "function",
    "objective",
    "constraint",
  ],
  shapes: shapeTypes,
  constraints: Object.keys(constrDict),
  objectives: Object.keys(objDict),
  computations: Object.keys(compDict),
};

export const StyleLanguageTokens: languages.IMonarchLanguage = {
  ...styleCustoms,
  tokenizer: {
    root: [
      ...CommonTokens,
      [/[{}\[\]()]/, "@brackets"],
      [
        /[a-z_A-Z$][\w$]*/,
        {
          cases: {
            "@keywords": "keyword",
            "@computations": "constructor",
            "@objectives": "constructor",
            "@constraints": "constructor",
            "@shapes": "tag",
            "@types": "type",
            "@default": "identifier",
          },
        },
      ],
      [
        /\b[+-]?(?:\d+(?:[.]\d*)?(?:[eE][+-]?\d+)?|[.]\d+(?:[eE][+-]?\d+)?)\b/,
        "number.float",
      ],
      [/#([0-9A-Fa-f]{3,})/, "number.hex"],
      { include: "@whitespace" },
    ],
    ...CommentCommon,
  },
};

export const StyleCompletions = (range: IRange): languages.CompletionItem[] => [
  ...styleCustoms.keywords.map((keyword: string) => ({
    label: keyword,
    insertText: keyword,
    kind: languages.CompletionItemKind.Keyword,
    range,
  })),
  ...styleCustoms.shapes.map((keyword: string) => ({
    label: keyword,
    insertText: `${keyword} {
  $0
}`,
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.Class,
    detail: "shape constructor",
    range,
  })),
  ...styleCustoms.computations.map((keyword: string) => ({
    label: keyword,
    insertText: `${keyword}($0)`,
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.Method,
    detail: "computation",
    range,
  })),
  ...styleCustoms.constraints.map((keyword: string) => ({
    label: keyword,
    insertText: `${keyword}($0)`,
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.Event,
    detail: "constraint",
    range,
  })),
  ...styleCustoms.objectives.map((keyword: string) => ({
    label: keyword,
    insertText: `${keyword}($0)`,
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.Event,
    detail: "objective",
    range,
  })),
  ...styleCustoms.types.map((keyword: string) => ({
    label: keyword,
    insertText: keyword,
    kind: languages.CompletionItemKind.TypeParameter,
    detail: "type",
    range,
  })),
];

export const SetupStyleMonaco = (monaco: Monaco) => {
  monaco.languages.register({ id: "style" });
  monaco.languages.setLanguageConfiguration("style", StyleConfig);
  monaco.languages.setMonarchTokensProvider("style", StyleLanguageTokens);
  const disposeColor = monaco.languages.registerColorProvider("style", {
    provideColorPresentations: (model, colorInfo) => {
      const { red, green, blue, alpha } = colorInfo.color;
      return [
        {
          label: rgbaToHex([red, green, blue, alpha]),
        },
      ];
    },

    provideDocumentColors: (model) => {
      const colorRegex = /#([0-9A-Fa-f]{3,})/;
      const colorMatches = model.findMatches(
        colorRegex.source,
        false,
        true,
        false,
        null,
        true,
      );
      return colorMatches.reduce(
        (
          colors: languages.IColorInformation[],
          { matches, range }: editor.FindMatch,
        ) => {
          if (matches !== null) {
            const hexColor = hexToRgba(matches[1]);
            if (hexColor) {
              const [red, green, blue, alpha] = hexColor;
              const color = {
                color: {
                  red,
                  green,
                  blue,
                  alpha,
                },
                range: range,
              };
              return [...colors, color];
            } else {
              return colors;
            }
          } else {
            return colors;
          }
        },
        [],
      );
    },
  });
  const disposeCompletion = monaco.languages.registerCompletionItemProvider(
    "style",
    {
      provideCompletionItems: (model, position) => {
        const word = model.getWordUntilPosition(position);
        const range: IRange = {
          startLineNumber: position.lineNumber,
          endLineNumber: position.lineNumber,
          startColumn: word.startColumn,
          endColumn: word.endColumn,
        };
        return { suggestions: StyleCompletions(range) } as any;
      },
    },
  );
  return () => {
    disposeColor.dispose();
    disposeCompletion.dispose();
  };
};
