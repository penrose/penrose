import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { parser } from "./substance.js";

export const substanceLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      styleTags({
        LineComment: t.lineComment,
        BlockComment: t.lineComment,
        for: t.keyword,
        in: t.keyword,
        where: t.keyword,
        Let: t.keyword,
        All: t.keyword,
        AutoLabel: t.keyword,
        Label: t.keyword,
        NoLabel: t.keyword,
        // TeX: t.string,
        // String: t.string,
        "NamedId/...": t.typeName,
      }),
    ],
  }),
});

export const substanceLanguageSupport = () => {
  return new LanguageSupport(substanceLanguage);
};
