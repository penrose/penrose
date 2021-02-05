import shapeMap from "./shapeMap";

const RenderShape = (shape: IShape) => {
  if (!(shape.shapeType in shapeMap)) {
    console.error(`${shape.shapeType} shape doesn't exist in shapeMap`);
    return null;
  }
  return shapeMap[shape.shapeType](shape);
};

const RenderStatic = (shapes: IShape[], canvasSize: [number, number]) => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("version", "1.2");
  svg.setAttribute("viewbox", `0 0 ${canvasSize[0]} ${canvasSize[1]}`);
  shapes.forEach((shape) => svg.appendChild(RenderShape(shape)));
};

export default RenderStatic;
