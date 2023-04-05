import { Monaco } from "@monaco-editor/react";
import {
  compDict,
  constrDict,
  hexToRgba,
  objDict,
  rgbaToHex,
  shapedefs,
} from "@penrose/core";
import { editor, IRange, languages } from "monaco-editor";
import { CommentCommon, CommonTokens } from "./common";

// import shapeDefs from "../shapedefs.json";
import shapedefsJson from "../../../../docs-site/src/shapedefs.json";
import { logAD } from "@penrose/core/dist/engine/Autodiff";
import { ShapeDef } from "@penrose/core/dist/shapes/Shapes";

// prevent more than 1 hover from appearing at once
// *mostly* works, seems to behave strangely out of nowhere sometimes
let registeredHover = false;

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
    "in",
    "except",
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
  properties: getShapeProperties(),
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

  if (!registeredHover) {
    monaco.languages.registerHoverProvider("style", {
      provideHover: (model, position) => {
        let word = model.getWordAtPosition(position)?.word
        return StyleHelpResolver(word)
      }
    })
    registeredHover = true;
  }

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
    }
  );
  return () => {
    disposeColor.dispose();
    disposeCompletion.dispose();
  };
};

interface Dictionary<T> {
  [Key: string]: T;
}

function getShapeProperties() {
  let shapes = Object.entries(shapedefs);
  let huh: string[] = [];
  shapes.forEach(elem => {
    let shape = elem[0];
    let tags = elem[1].propTags

    Object.entries(tags).forEach(tag => {
      let tagName = tag[0]
      huh.push(tagName)

    })

  })

  return huh;
}


function genPropertyTypes() {
  let what: Dictionary<string> = {};
  let shapes = Object.entries(shapedefs);

  shapes.forEach(elem => {
    let shape = elem[0];
    let tags = elem[1].propTags

    Object.entries(tags).forEach(tag => {
      let tagName = tag[0]
      let tagType = tag[1]

      what[tagName] = `${tagType}`

    })

  })

  return what
}

// kinda lazy but it works
function createFlattenedStyleCustoms() {
  let bruh = Object.entries(styleCustoms);
  let huh: Dictionary<string> = {};

  bruh.forEach(element => {
    const [key, val] = element
    val.forEach((wrd) => {
      huh[wrd] = key
    })
  });
  return huh
}


const flattenedStyleCustoms = { ...createFlattenedStyleCustoms() };
const propertyTypes = genPropertyTypes();


export function StyleHelpResolver(hoveredWord: string | undefined) {
  if (hoveredWord === undefined) return {
    contents: []
  };

  let help, type;
  console.log({hoveredWord, styleCustoms});


  switch (flattenedStyleCustoms[hoveredWord]) {
    case 'shapes':
      type = 'shape'
      help = `properties:\n ${propsToMarkDown((shapedefs as any)[hoveredWord]['propTags'])}`;
      break;

    case 'types':
      type = 'type'
      help = `usage:\n ${typeUsage[hoveredWord]}`
      break;

    case 'keywords':
      type = 'keyword'
      break;

    case 'constraints':
      type = 'constraint'
      help = functionHelp(hoveredWord)
      break;

    case 'objectives':
      type = 'objective'
      help = functionHelp(hoveredWord)
      break;

    case 'computations':
      type = 'computation'
      help = functionHelp(hoveredWord)
      break;

    case 'properties':
      type = `property of type ${propertyTypes[hoveredWord]}`
      help = `usage: ${typeUsage[propertyTypes[hoveredWord]]}`
      break;

    default:
      type = ''
      break;
  }

  help ||= "no help found (╥_╥)";

  return {
    contents: [
      { value: `**${hoveredWord}** : ${type}` },
      {
        value: help
      }
    ]
  };
}


function propsToMarkDown(props: Object) {
  let retStr = ""

  for (const [k, v] of Object.entries(props)) {
    retStr += `- ${k} : ${v}\n`
  }

  return retStr;
}

// will likely extend but this will suffice for now
function functionHelp(functionName:string) {
  return `[documentation](https://penrose.cs.cmu.edu/docs/ref/style/functions#${functionName.toLowerCase()})`
}


const typeUsage: Dictionary<string> = {
  "StrV": `string | string + string + ...`,
  "FloatV": `number | ?`,
  "ListV": `[1,2,...] | [1,?,...]`,
  "LListV": `[[1,2,...],[2,3,...],...] | [[1,?],...]`,
  "VectorV": `(1,2,...) | (1,?,...)`,
  "MatrixV": `((1,2,...),(4,5,...),(7,8,...))`,
  "TupV": `{1,2,...} | {1,?,...}`,
  "ColorV": `hsva(h,s,v,a) | rgba(r,g,b,a) | #rrggbbaa`,
  "BoolV": `true | false`,
  "ShapeListV": `[t.shape1, t.shape2, ..., t.shapeN], where \`t.shape1\`, \`t.shape2\`, ..., \`t.shape3\` are all previously-defined shapes`,
  "PtListV": ``, // "a matrix, or a list of lists", smh
}