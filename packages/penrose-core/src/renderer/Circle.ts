import { toHex, toScreen } from "utils/Util";

const DASH_ARRAY = "7,5";

const Circle = (shape: IShape, canvasSize: [number, number]) => {
  const { properties } = shape;
  const center = properties.center as IVectorV<number>;
  const r = properties.r as IFloatV<number>;
  const color = properties.color as IColorV<number>;
  const alpha = color.contents.contents[3];
  const strokeColor = properties.strokeColor as IColorV<number>;
  const strokeAlpha = strokeColor.contents.contents[3];
  const thickness = properties.strokeWidth.contents;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);

  const elem = document.createElementNS("http://www.w3.org/2000/svg", "circle");
  elem.setAttribute("cx", x.toString());
  elem.setAttribute("cy", y.toString());
  elem.setAttribute("r", r.contents.toString());
  elem.setAttribute("fill", toHex(color.contents));
  elem.setAttribute("fill-opacity", alpha.toString());
  elem.setAttribute("stroke", toHex(strokeColor.contents));
  elem.setAttribute("stroke-opacity", strokeAlpha.toString());
  elem.setAttribute("stroke-width", thickness.toString());

  let dashArray = DASH_ARRAY;
  if ("strokeDashArray" in properties) {
    dashArray = (properties.strokeDashArray as IStrV<string>).contents;
  }
  if (properties.strokeStyle.contents === "dashed") {
    elem.setAttribute("strokeDasharray", dashArray.toString());
  }

  const name = shape.properties.name as IStrV<string>;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);

  return elem;
};
export default Circle;
