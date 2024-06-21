// Edited version of https://uiwjs.github.io/react-codemirror/#/theme/data/quietlight

import { tags as t } from "@lezer/highlight";
import { createTheme, type CreateThemeOptions } from "@uiw/codemirror-themes";

export const config = {
  background: "#F5F5F5",
  foreground: "#333333",
  selection: "#C9D0D9",
  selectionMatch: "#C9D0D9",
  cursor: "#54494B",
  dropdownBackground: "#F5F5F5",
  activeLine: "#79ff002b",
  matchingBracket: "#E4F6D4",
  keyword: "#4B69C6",
  storage: "#4B69C6",
  variable: "#db8523",
  parameter: "#7A3E9D",
  function: "#AA3731",
  string: "#448C27",
  constant: "#9C5D27",
  type: "#7A3E9D",
  class: "#AA3731",
  number: "#9C5D27",
  comment: "#AAAAAA",
  heading: "#AA3731",
  invalid: "#cd3131",
  regexp: "#4B69C6",
  tag: "#4B69C6",
};
export const defaultSettingsPenroseTheme: CreateThemeOptions["settings"] = {
  background: config.background,
  foreground: config.foreground,
  caret: config.cursor,
  selection: config.selection,
  selectionMatch: config.selection,
  gutterBackground: config.background,
  gutterForeground: config.foreground,
  gutterBorder: "transparent",
  lineHighlight: config.activeLine,
};

export const penroseThemeInit = (options?: Partial<CreateThemeOptions>) => {
  const { theme = "light", settings = {}, styles = [] } = options || {};
  return createTheme({
    theme: theme,
    settings: {
      ...defaultSettingsPenroseTheme,
      ...settings,
    },
    styles: [
      { tag: t.keyword, color: config.keyword },
      {
        tag: [t.name, t.deleted, t.character, t.macroName],
        color: config.variable,
      },
      { tag: [t.propertyName], color: config.function },
      {
        tag: [
          t.processingInstruction,
          t.string,
          t.inserted,
          t.special(t.string),
        ],
        color: config.string,
      },
      {
        tag: [t.function(t.variableName), t.labelName],
        color: config.function,
      },
      {
        tag: [t.color, t.constant(t.name), t.standard(t.name)],
        color: config.constant,
      },
      { tag: [t.definition(t.name), t.separator], color: config.variable },
      { tag: [t.className], color: config.class },
      {
        tag: [
          t.number,
          t.changed,
          t.annotation,
          t.modifier,
          t.self,
          t.namespace,
        ],
        color: config.number,
      },
      { tag: [t.typeName], color: config.type, fontStyle: config.type },
      { tag: [t.operator, t.operatorKeyword], color: config.keyword },
      { tag: [t.url, t.escape, t.regexp, t.link], color: config.regexp },
      { tag: [t.meta, t.comment], color: config.comment },
      { tag: t.tagName, color: config.tag },
      { tag: t.strong, fontWeight: "bold" },
      { tag: t.emphasis, fontStyle: "italic" },
      { tag: t.link, textDecoration: "underline" },
      { tag: t.heading, fontWeight: "bold", color: config.heading },
      {
        tag: [t.atom, t.bool, t.special(t.variableName)],
        color: config.variable,
      },
      { tag: t.invalid, color: config.invalid },
      { tag: t.strikethrough, textDecoration: "line-through" },
      ...styles,
    ],
  });
};

export const penroseTheme = penroseThemeInit();
