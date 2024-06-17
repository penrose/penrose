import { ContextTracker, ExternalTokenizer } from "@lezer/lr";
import {
  BlockComment,
  LineComment,
  insertSemi,
  newline,
  spaces,
} from "./style.terms.js";

export const trackNewline = new ContextTracker({
  start: false,
  shift(context, term) {
    return term === LineComment || term === BlockComment || term === spaces
      ? context
      : term === newline;
  },
  strict: false,
});

export const insertSemicolon = new ExternalTokenizer(
  (input, stack) => {
    let { next } = input;
    if (next === -1 || stack.context) input.acceptToken(insertSemi);
  },
  { contextual: true, fallback: true },
);
