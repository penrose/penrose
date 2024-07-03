import { syntaxTree } from "@codemirror/language";
import { EditorView, hoverTooltip } from "@codemirror/view";
import { compDict, constrDict, isKeyOf } from "@penrose/core";
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
  let start = pos,
    end = pos;
  while (start > from && /\w/.test(text[start - from - 1])) start--;
  while (end < to && /\w/.test(text[end - from])) end++;
  if ((start == pos && side < 0) || (end == pos && side > 0)) return null;

  //   Avoids tooltip showing in Expr part of function calls
  //   If more tooltips are added this check may be problematic
  if (cursor.prevSibling()) return null;
  // Go to parent since ComputationFn, ObjConstrBody, ShapeName have Identifier inside
  cursor.parent();

  // Computation & Constraint Function tooltip
  cursor.firstChild();
  const fnName = extractText(view.state.doc.toString(), cursor.to, cursor.from);

  if (isKeyOf(fnName, compDict)) {
    let text = toParamString(compDict[fnName], fnName);
    return createTooltip(start, end, text);
  }

  if (isKeyOf(fnName, constrDict)) {
    let text = toParamString(constrDict[fnName], fnName);
    return createTooltip(start, end, text);
  }

  cursor.parent();

  // Shape name show properties tooltip
  if (cursor.name === "ShapeName") {
    const shapeName = extractText(
      view.state.doc.toString(),
      cursor.to,
      cursor.from,
    );

    const shapeDefs = getShapeDefs();

    if (shapeName in shapeDefs) {
      let text =
        shapeName +
        " Properties: " +
        Object.keys(shapeDefs[shapeName]).join(", ");
      return createTooltip(start, end, text);
    }
  }

  //   Shape property show type tooltip
  if (cursor.name === "PropName") {
    const propName = extractText(
      view.state.doc.toString(),
      cursor.to,
      cursor.from,
    );
    const shapeDefs = getShapeDefs();
    console.log(shapeDefs);

    if (
      traverseCursorUp(cursor, "ShapeDecl") &&
      traverseCursorDown(cursor, "Identifier")
    ) {
      const shapeName = extractText(
        view.state.doc.toString(),
        cursor.to,
        cursor.from,
      );

      if (shapeName in shapeDefs && propName in shapeDefs[shapeName]) {
        const text = `${propName}:${shapeDefs[shapeName][propName]}`;
        return createTooltip(start, end, text);
      }
    }
  }

  return null;
});
