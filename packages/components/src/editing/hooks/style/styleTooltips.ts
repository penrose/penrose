import { syntaxTree } from "@codemirror/language";
import { EditorView, hoverTooltip } from "@codemirror/view";
import { compDict, constrDict, isKeyOf, objDict } from "@penrose/core";
import {
  extractText,
  getShapeDefs,
  toParamString,
  traverseCursorDown,
  traverseCursorUp,
} from "../hooksUtils";

const createTooltip = (start: number, end: number, text: string) => {
  return {
    pos: start,
    end,
    above: true,
    create(view: EditorView) {
      let dom = document.createElement("div");
      dom.classList.add("tooltip");
      dom.textContent = text;
      return { dom };
    },
  };
};

export const wordHover = hoverTooltip((view, pos, side) => {
  let { from, to, text } = view.state.doc.lineAt(pos);
  let cursor = syntaxTree(view.state).cursorAt(pos);

  // Determine word boundaries: https://codemirror.net/examples/tooltip/
  // Necessary to determine tooltip show location
  let start = pos,
    end = pos;
  while (start > from && /\w/.test(text[start - from - 1])) start--;
  while (end < to && /\w/.test(text[end - from])) end++;
  if ((start == pos && side < 0) || (end == pos && side > 0)) return null;

  const word = extractText(view.state.doc.toString(), cursor.to, cursor.from);
  console.log(word);

  if (isKeyOf(word, compDict)) {
    let text = toParamString(compDict[word], word);
    return createTooltip(start, end, text);
  }

  if (isKeyOf(word, objDict)) {
    let text = toParamString(objDict[word], word);
    return createTooltip(start, end, text);
  }

  if (isKeyOf(word, constrDict)) {
    console.log("here");
    let text = toParamString(constrDict[word], word);
    return createTooltip(start, end, text);
  }

  // Go to parent since ShapeName and PropName have Identifier inside
  cursor.parent();

  // Shape name show properties tooltip
  if (cursor.name === "ShapeName") {
    const shapeDefs = getShapeDefs();

    if (word in shapeDefs) {
      let text =
        word + " Properties: " + Object.keys(shapeDefs[word]).join(", ");
      return createTooltip(start, end, text);
    }
  }

  // Shape property show type tooltip
  if (cursor.name === "PropName") {
    const shapeDefs = getShapeDefs();

    if (
      traverseCursorUp(cursor, "ShapeDecl") &&
      traverseCursorDown(cursor, "Identifier")
    ) {
      const shapeName = extractText(
        view.state.doc.toString(),
        cursor.to,
        cursor.from,
      );

      if (shapeName in shapeDefs && word in shapeDefs[shapeName]) {
        const text = `${word}:${shapeDefs[shapeName][word]}`;
        return createTooltip(start, end, text);
      }
    }
  }

  return null;
});
