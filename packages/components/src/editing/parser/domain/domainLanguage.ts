import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { parser } from "./domain.js";

export const domainLanguage = LRLanguage.define({
  languageData: {
    commentTokens: { line: "--", block: { open: "/*", close: "*/" } },
  },
  parser: parser.configure({
    props: [
      styleTags({
        LineComment: t.lineComment,
        BlockComment: t.lineComment,
        type: t.keyword,
        predicate: t.keyword,
        symmetric: t.keyword,
        function: t.keyword,
        constructor: t.keyword,
      }),
    ],
  }),
});

export const domainLanguageSupport = () => {
  return new LanguageSupport(domainLanguage);
};
