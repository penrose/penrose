import { syntaxTree } from "@codemirror/language";
import { EditorView, hoverTooltip } from "@codemirror/view";
import { compDict, constrDict, isKeyOf, objDict } from "@penrose/core";
import Markdown from "markdown-it";
import markdownItKatex from "markdown-it-katex";
import {
  convertShapeProps,
  extractText,
  getShapeDefs,
  toParamString,
  traverseCursorDown,
  traverseCursorUp,
} from "../hooksUtils";

const createTooltip = (start: number, end: number, dom: HTMLDivElement) => {
  return {
    pos: start,
    end,
    above: true,
    create(view: EditorView) {
      return { dom };
    },
  };
};

const createFunctionDOM = (dict: any, name: string) => {
  let paramStr = toParamString(dict, name);
  let desc = dict.description ?? "";

  const md = Markdown();
  md.use(markdownItKatex);
  var result = md.render(desc);

  let dom = document.createElement("div");
  dom.classList.add("tooltip");
  dom.innerHTML =
    '<span style="color: #4B69C6;">' + paramStr + "</span><br>" + result;
  return dom;
};

const createPropertiesDOM = (shapeProps: string[], name: string) => {
  let dom = document.createElement("div");
  dom.classList.add("tooltip");
  dom.innerHTML =
    '<span style="color: #4B69C6;">' + name + " Properties:" + "</span>";

  const ul = document.createElement("ul");
  ul.style.margin = "0 10px";
  ul.style.padding = "0 10px";

  shapeProps.forEach((prop) => {
    const li = document.createElement("li");
    li.textContent = prop;
    ul.appendChild(li);
  });

  dom.appendChild(ul);
  return dom;
};

const createShapePropDOM = (propName: string, type: string) => {
  let dom = document.createElement("div");
  dom.classList.add("tooltip");
  dom.innerHTML =
    '<span style="color: #4B69C6;">' + propName + " : " + "</span>" + type;
  return dom;
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

  if (isKeyOf(word, compDict)) {
    let domElt = createFunctionDOM(compDict[word], word);
    return createTooltip(start, end, domElt);
  }

  if (isKeyOf(word, objDict)) {
    let domElt = createFunctionDOM(objDict[word], word);
    return createTooltip(start, end, domElt);
  }

  if (isKeyOf(word, constrDict)) {
    let domElt = createFunctionDOM(constrDict[word], word);
    return createTooltip(start, end, domElt);
  }

  // Go to parent since ShapeName and PropName have Identifier inside
  cursor.parent();

  // Shape name show properties tooltip
  if (cursor.name === "ShapeName") {
    const shapeDefs = getShapeDefs();

    if (word in shapeDefs) {
      let domElt = createPropertiesDOM(Object.keys(shapeDefs[word]), word);
      return createTooltip(start, end, domElt);
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
        let domElt = createShapePropDOM(
          word,
          convertShapeProps(shapeDefs[shapeName][word]),
        );
        return createTooltip(start, end, domElt);
      }
    }
  }

  return null;
});
