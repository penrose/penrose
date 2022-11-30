import { Monaco } from "@monaco-editor/react";
import {
  compDict,
  constrDict,
  hexToRgba,
  objDict,
  rgbaToHex,
  shapedefs,
} from "@penrose/core";
import { ShapeType } from "@penrose/core/build/dist/shapes/Shapes";
import { editor, IRange, languages } from "monaco-editor";
import { CommentCommon, CommonTokens } from "./common";

interface Global {
  tag: "Global";
}
interface Block {
  tag: "Block";
}
interface ShapeConstructor {
  tag: "ShapeConstructor";
  shapeName: string;
}

type StyleScope = Global | Block | ShapeConstructor;

const guessScope = (prefixText: String): StyleScope => {
  // HACK: count braces and capture shape name
  const stack = [];
  for (const c of prefixText) {
    if (c === "{") {
      const shapeRegexStr = `(${Object.keys(shapedefs).join(
        "|"
      )})[\\n\\r\\s]*\\{`;
      const shapeRegex = new RegExp(shapeRegexStr, "g");
      const matches = prefixText.matchAll(shapeRegex);
      if (matches === null) {
        stack.push(c);
      } else {
        let shapeName;
        for (const match of matches) {
          shapeName = match[1];
        }
        stack.push(shapeName);
      }
    } else if (c === "}") {
      stack.pop();
    }
  }
  // figure out scope
  if (stack.length === 0) {
    return { tag: "Global" };
  } else if (stack.length === 1) {
    return { tag: "Block" };
  } else {
    // 2 levels
    return { tag: "ShapeConstructor", shapeName: stack.at(-1)! };
  }
};

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
    "where",
    "with",
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
  shapes: Object.keys(shapedefs),
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

export const StyleCompletions = (
  range: IRange,
  scope: StyleScope,
  context: languages.CompletionContext
): languages.CompletionItem[] => {
  switch (scope.tag) {
    case "Global":
    case "Block":
      if (
        context.triggerKind !== languages.CompletionTriggerKind.TriggerCharacter
      ) {
        return defaultCompletions(range);
      } else {
        return [];
      }
    case "ShapeConstructor": {
      const { shapeName } = scope;
      // HACK: need to validate shapeName first
      const def = shapedefs[shapeName as ShapeType];
      return [
        ...Object.entries(def.propTags).map(([prop, type]) => ({
          label: prop,
          detail: type,
          insertText: prop,
          kind: languages.CompletionItemKind.Property,
          range,
        })),
      ];
    }
  }
};

const defaultCompletions = (range: IRange): languages.CompletionItem[] => [
  ...styleCustoms.keywords.map((keyword: string) => ({
    label: keyword,
    insertText: keyword,
    kind: languages.CompletionItemKind.Keyword,
    range,
  })),
  ...styleCustoms.shapes.map((keyword: string) => ({
    label: keyword,
    insertText: `${keyword} {$0}`,
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
        true
      );
      return colorMatches.reduce(
        (
          colors: languages.IColorInformation[],
          { matches, range }: editor.FindMatch
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
        []
      );
    },
  });

  const disposeCompletion = monaco.languages.registerCompletionItemProvider(
    "style",
    {
      triggerCharacters: ["\n"],
      provideCompletionItems: (model, position, context) => {
        const word = model.getWordUntilPosition(position);
        const range: IRange = {
          startLineNumber: position.lineNumber,
          endLineNumber: position.lineNumber,
          startColumn: word.startColumn,
          endColumn: word.endColumn,
        };
        const prefix = model.getValueInRange({
          startLineNumber: 1,
          endLineNumber: position.lineNumber,
          startColumn: 1,
          endColumn: position.column,
        });
        const scope = guessScope(prefix);
        return { suggestions: StyleCompletions(range, scope, context) };
      },
    }
  );
  return () => {
    disposeColor.dispose();
    disposeCompletion.dispose();
  };
};
