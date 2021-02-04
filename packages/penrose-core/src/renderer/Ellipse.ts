import {
  useCenter,
  useFill,
  useRadii,
  useStroke,
  useTitle,
} from "./AttrHelper";

const Ellipse = (shape: IShape, canvasSize: [number, number]) => {
  const { properties } = shape;

  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "ellipse"
  );
  useFill(shape, elem);
  useCenter(shape, canvasSize, elem);
  useRadii(shape, elem);
  useStroke(shape, elem);
  useTitle(shape, elem);

  return elem;
};
export default Ellipse;
