import { languages } from "monaco-editor-core";

export const CommentCommon: any = {
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

export const CommonTokens: languages.IMonarchLanguageRule[] = [
  [/"(?:[^\n"]|\\["\\ntbfr])*"/, "string"],
];
