import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { parser } from "./style.js";

export const styleLanguage = LRLanguage.define({
  languageData: {
    commentTokens: { line: "--", block: { open: "/*", close: "*/" } },
  },
  parser: parser.configure({
    props: [
      styleTags({
        LineComment: t.lineComment,
        BlockComment: t.lineComment,
        type: t.keyword,
        "Namespace/...": t.namespace,
        "SubVar/...": t.string,
        "StyVar/...": t.variableName,
        "ShapeName/...": t.className,
        "PropName/...": t.propertyName,
        "Type/...": t.typeName,
        // keywords
        collect: t.keyword,
        forall: t.keyword,
        with: t.keyword,
        where: t.keyword,
        repeatable: t.keyword,
        foreach: t.keyword,
        as: t.keyword,
        into: t.keyword,
        listof: t.keyword,
        nameof: t.keyword,
        numberof: t.keyword,
        from: t.keyword,
        ensure: t.keyword,
        encourage: t.keyword,
        layer: t.keyword,
        above: t.keyword,
        below: t.keyword,
        // variables
        ShapeName: t.className,
        StyVar: t.variableName,
        // constants
        Number: t.number,
        String: t.string,
        ColorLiteral: t.color,
        BooleanLiteral: t.bool,
        // delimiters
        LParen: t.paren,
        RParen: t.paren,
        Bind: t.operator,
      }),
    ],
  }),
});

export const styleLanguageSupport = () => {
  return new LanguageSupport(styleLanguage);
};
