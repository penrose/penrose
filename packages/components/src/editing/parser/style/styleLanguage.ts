import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { parser } from "./style.js";

export const styleLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      styleTags({
        LineComment: t.lineComment,
        BlockComment: t.lineComment,
        type: t.keyword,
        "Namespace/...": t.namespace,
        "SubVar/...": t.string,
        "StyVar/...": t.variableName,
        // keywords
        collect: t.keyword,
        forall: t.keyword,
        with: t.keyword,
        where: t.keyword,
        repeatable: t.keyword,
        foreach: t.keyword,
        as: t.keyword,
        into: t.keyword,
        StyVar: t.variableName,
        // constants
        Number: t.number,
        String: t.string,
        // delimiters
        LParen: t.paren,
        RParen: t.paren,
        Bind: t.operator,
        // predicate: t.keyword,
        // symmetric: t.keyword,
        // function: t.keyword,
        // constructor: t.keyword,
      }),
    ],
  }),
});

export const styleLanguageSupport = () => {
  return new LanguageSupport(styleLanguage);
};
