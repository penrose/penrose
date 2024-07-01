import { syntaxTree } from "@codemirror/language";
import { hoverTooltip } from "@codemirror/view";
import { extractText, getShapeDefs } from "../hooksUtils";

export const wordHover = hoverTooltip((view, pos, side) => {
  let { from, to, text } = view.state.doc.lineAt(pos);
  let cursor = syntaxTree(view.state).cursorAt(pos);

  // Determine word boundaries: https://codemirror.net/examples/tooltip/
  let start = pos,
    end = pos;
  while (start > from && /\w/.test(text[start - from - 1])) start--;
  while (end < to && /\w/.test(text[end - from])) end++;
  if ((start == pos && side < 0) || (end == pos && side > 0)) return null;

  // Check for Shape Name
  // Go to parent since ShapeName has Identifier inside
  cursor.parent();
  if (cursor.name === "ShapeName") {
    const shapeName = extractText(
      view.state.doc.toString(),
      cursor.to,
      cursor.from,
    );

    const shapeDefs = getShapeDefs();

    console.log(shapeDefs);
    if (shapeName in shapeDefs) {
      return {
        pos: start,
        end,
        above: true,
        create(view) {
          let dom = document.createElement("div");
          dom.classList.add("tooltip");
          dom.textContent =
            shapeName +
            " Properties: " +
            Object.keys(shapeDefs[shapeName]).join(", ");
          return { dom };
        },
      };
    }
  }

  return null;
});
