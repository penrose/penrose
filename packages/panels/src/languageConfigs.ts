import { languages, IRange } from "monaco-editor";

const CommentCommon: any = {
  comment: [
    [/[^/*]+/, "comment"],
    [/\/\*/, "comment", "@push"], // nested comment
    ["\\*/", "comment", "@pop"],
    [/[/*]/, "comment"],
  ],
  whitespace: [
    [/[ \t\r\n]+/, "white"],
    [/\/\*/, "comment", "@comment"],
    [/--.*?$/, "comment"],
  ],
};

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

const CommonTokens: languages.IMonarchLanguageRule[] = [
  [/"(?:[^\n"]|\\["\\ntbfr])*"/, "string"],
];

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
  shapes: [
    "Arrow",
    "Path",
    "Line",
    "Text",
    "Square",
    "Image",
    "Rectangle",
    "Circle",
  ],
  constraints: [
    "atDist",
    "contains",
    "disjoint",
    "disjointScalar",
    "equal",
    "inRange",
    "lessThan",
    "lessThanSq",
    "maxSize",
    "minSize",
    "notCrossing",
    "outsideOf",
    "overlapping",
    "perpendicular",
    "rightwards",
    "smallerThan",
    "tangentTo",
  ],
  objectives: [
    "above",
    "below",
    "centerArrow",
    "centerLabel",
    "centerLabelAbove",
    "equal",
    "near",
    "nearPt",
    "nearScalar",
    "repel",
    "repelScalar",
    "sameCenter",
  ],
  computations: [
    "abs",
    "average",
    "average2",
    "cos",
    "derivative",
    "derivativePreconditioned",
    "dot",
    "get",
    "hsva",
    "intersectingSideSize",
    "len",
    "lineLength",
    "max",
    "midpointOffset",
    "min",
    "mul",
    "norm",
    "normalize",
    "normsq",
    "orientedSquare",
    "pathFromPoints",
    "rgba",
    "rot90",
    "sampleColor",
    "setOpacity",
    "sin",
    "sqrt",
    "triangle",
    "unit",
    "unitMark",
    "unitMark2",
    "vdist",
    "vdistsq",
  ],
};

export const StyleLanguageTokens: languages.IMonarchLanguage = {
  ...styleCustoms,
  tokenizer: {
    root: [
      ...CommonTokens,
      [/[{}()[]]/, "@brackets"],
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

export const SubstanceLanguageTokens = (
  domainCache: any
): languages.IMonarchLanguage => {
  const refs = {
    types: [...domainCache.types.keys()],
    functionLikes: [
      ...domainCache.constructors.keys(),
      ...domainCache.functions.keys(),
      ...domainCache.predicates.keys(),
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
  const types = [...domainCache.types.keys()].map((type) => ({
    label: type,
    insertText: type + " $0",
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.TypeParameter,
    detail: "type",
    range,
  }));
  const predicates = [...domainCache.predicates.keys()].map((type) => ({
    label: type,
    insertText: type + "($0)",
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.Property,
    detail: "predicate",
    range,
  }));
  const constructors = [...domainCache.constructors.keys()].map((type) => ({
    label: type,
    insertText: type + "($0)",
    insertTextRules: languages.CompletionItemInsertTextRule.InsertAsSnippet,
    kind: languages.CompletionItemKind.Constructor,
    detail: "constructor",
    range,
  }));
  const labeling = ["AutoLabel", "Label", "NoLabel", "All"].map((type) => ({
    label: type,
    insertText: type,
    kind: languages.CompletionItemKind.Color,
    detail: "labeling",
    range,
  }));

  const fns = [...domainCache.functions.entries()].map(([name, fn]) => ({
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
  }));

  return [...types, ...fns, ...predicates, ...constructors, ...labeling];
};
