import { syntaxTree } from "@codemirror/language";
import { EditorView, hoverTooltip } from "@codemirror/view";
import { compDict, constrDict, isKeyOf } from "@penrose/core";
import { extractText, getShapeDefs } from "../hooksUtils";

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

const toParamString = (dict: any, name: string) => {
  console.log(dict.params);
  if (!dict.params || !Array.isArray(dict.params)) {
    return "";
  }

  const formattedParams = dict.params
    .map((param: any) => `${param.name}: ${param.type.type}`)
    .join(", ");
  return `${name}(${formattedParams})`;
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
  if (
    cursor.name === "ComputationFunction" ||
    cursor.name === "ObjConstrBody"
  ) {
    console.log("here");
    let name = cursor.name;
    cursor.firstChild();
    const fnName = extractText(
      view.state.doc.toString(),
      cursor.to,
      cursor.from,
    );

    if (name === "ComputationFunction") {
      // Necessary to avoid type error
      if (!isKeyOf(fnName, compDict)) return null;
      let text = toParamString(compDict[fnName], fnName);
      return createTooltip(start, end, text);
    }

    // Constraint Function case
    if (name === "ObjConstrBody") {
      console.log("inside");
      if (!isKeyOf(fnName, constrDict)) return null;
      let text = toParamString(constrDict[fnName], fnName);
      return createTooltip(start, end, text);
    }
  }

  // Shape property tooltip
  if (cursor.name === "ShapeName") {
    const shapeName = extractText(
      view.state.doc.toString(),
      cursor.to,
      cursor.from,
    );

    const shapeDefs = getShapeDefs();

    // console.log(shapeDefs);
    if (shapeName in shapeDefs) {
      let text =
        shapeName +
        " Properties: " +
        Object.keys(shapeDefs[shapeName]).join(", ");
      return createTooltip(start, end, text);
    }
  }

  return null;
});
