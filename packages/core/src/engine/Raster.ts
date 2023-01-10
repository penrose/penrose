import { CircleProps } from "shapes/Circle";
import { Shape } from "shapes/Shapes";

const rasterizeCircle = ({ r, center, strokeWidth }: CircleProps): void => {
  return;
};

export const rasterize = (shape: Shape): void => {
  switch (shape.shapeType) {
    case "Circle":
      rasterizeCircle(shape);
  }
};
