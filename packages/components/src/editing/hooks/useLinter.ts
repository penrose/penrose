import { syntaxTree } from "@codemirror/language";
import { Diagnostic } from "@codemirror/lint";
import { Text } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { errLocs, showError } from "@penrose/core";
import {
  DomainError,
  RuntimeError,
  StyleError,
  StyleErrorList,
  StyleWarning,
  SubstanceError,
} from "@penrose/core/dist/types/errors.js";
import { ErrorLoc } from "@penrose/core/dist/utils/Util.js";
import { useCallback } from "react";

// Copied from old code
type IndividualError = Exclude<
  StyleError | DomainError | SubstanceError | RuntimeError | StyleWarning,
  StyleErrorList
>;

type ErrLocPair = {
  err: IndividualError;
  loc: ErrorLoc;
};

const errLocPairs = (
  err: StyleError | DomainError | SubstanceError | RuntimeError | StyleWarning,
): ErrLocPair[] => {
  if (err.tag === "StyleErrorList") {
    const pairs = err.errors.map(errLocPairs);
    return pairs.flat(1);
  } else {
    const locs = errLocs(err);
    return locs.length > 0 ? [{ err, loc: locs[0] }] : [];
  }
};

// need to test this more
const getPos = (line: number, col: number, text: Text) => {
  // Rule seems to work correctly for all but end of line, min compensates
  const pos = text.line(line).from + col;
  return Math.min(pos, text.line(line).to);
};

const renderMarker = (
  message: string,
  languageType: "substance" | "style" | "domain",
) => {
  return (view: EditorView) => {
    let dom = document.createElement("div");

    let redirect = `https://penrose.cs.cmu.edu/docs/ref/${languageType}/overview`;
    dom.innerHTML = `<a href="${redirect}" target="_blank">` + message + "</a>";
    return dom;
  };
};

export const createLinter = (
  error: StyleError | DomainError | SubstanceError | RuntimeError | null,
  warnings: StyleWarning[],
  languageType: "substance" | "style" | "domain",
  showCompileErrs: boolean,
) => {
  return useCallback(
    async (view: EditorView) => {
      let diagnostics: Diagnostic[] = [];

      const errPairs = error === null ? [] : errLocPairs(error);
      const warningPairs = warnings.map(errLocPairs).flat(1);

      // Case 1: Compiler errors
      if (showCompileErrs && (errPairs.length > 0 || warningPairs.length > 0)) {
        let text = view.state.doc;

        const errMarkers = errPairs
          .filter(({ loc }) => loc.type.toLowerCase() === languageType)
          .map(({ loc, err }) => ({
            from: getPos(loc.range.start.line, loc.range.start.col, text),
            to: getPos(loc.range.end.line, loc.range.end.col, text),
            message: showError(err),
            renderMessage: renderMarker(showError(err), languageType),
            severity: "error",
          }));

        const warningMarkers = warningPairs
          .filter(({ loc }) => loc.type.toLowerCase() === languageType)
          .map(({ loc, err: warn }) => ({
            from: getPos(loc.range.start.line, loc.range.start.col, text),
            to: getPos(loc.range.end.line, loc.range.end.col, text),
            message: showError(warn),
            severity: "warning",
          }));

        // type for err should be Severity from lezer, but its not exported
        errMarkers.forEach((err: any) => {
          diagnostics.push(err);
        });

        warningMarkers.forEach((warn: any) => {
          diagnostics.push(warn);
        });

        // Case 2: Show parse errors before compile time / after edit
      } else {
        // Code below is to show parse errors before compile time
        syntaxTree(view.state).iterate({
          enter: (node) => {
            if (node.type.isError) {
              const from = node.from;
              const to = node.to;
              diagnostics.push({
                from,
                to,
                severity: "error",
                renderMessage: renderMarker("Syntax Error", languageType),
                message: "Syntax Error",
              });
            }
          },
        });
      }

      return diagnostics;
    },
    [error, warnings, languageType, showCompileErrs],
  );
};
