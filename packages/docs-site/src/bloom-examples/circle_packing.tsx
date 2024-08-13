import {
  DiagramBuilder,
  Renderer,
  SharedInput,
  canvas,
  constraints,
  useDiagram,
  useSharedInput,
} from "@penrose/bloom";
import { useCallback } from "react";

const circlePackingDiagram = async (inputs: {
  containerRad: SharedInput;
  circleRad: SharedInput;
}) => {
  const width = 400;
  const height = 400;
  const numCircles = 15;

  const { circle, ensure, type, forall, build, sharedInput, rectangle } =
    new DiagramBuilder(canvas(width, height), "", 1000);

  const circleRad = sharedInput(inputs.circleRad);
  const containerRad = sharedInput(inputs.containerRad);
  const containerBorder = 2;

  const Circle = type();
  const Enclosure = type();

  for (let i = 0; i < numCircles; ++i) {
    Circle();
  }

  const enclosure = Enclosure();
  enclosure.icon = circle({
    r: containerRad,
    center: [0, 0],
    strokeWidth: containerBorder,
    strokeColor: [0, 0, 0, 1],
    fillColor: [0, 0, 0, 0],
  });

  forall({ c: Circle }, ({ c }) => {
    c.icon = circle({
      r: circleRad,
      drag: true,
      dragConstraint: ([x, y]) => {
        const norm = Math.sqrt(x * x + y * y);
        const maxNorm =
          inputs.containerRad.get()! -
          containerBorder / 2 -
          inputs.circleRad.get()!;
        if (norm <= maxNorm) {
          return [x, y];
        } else {
          return [(x / norm) * maxNorm, (y / norm) * maxNorm];
        }
      },
    });

    c.icon.fillColor[3] = 1;

    ensure(constraints.contains(enclosure.icon, c.icon, containerBorder / 2));
  });

  forall({ c1: Circle, c2: Circle }, ({ c1, c2 }) => {
    ensure(constraints.disjoint(c1.icon, c2.icon));
  });

  return await build();
};

export default function CirclePackingDiagram() {
  const defaultContRad = 150;
  const defaultCircRad = 20;
  const containerRad = useSharedInput(defaultContRad);
  const circleRad = useSharedInput(defaultCircRad);

  const diagram = useDiagram(
    useCallback(
      () => circlePackingDiagram({ containerRad, circleRad }),
      [containerRad, circleRad],
    ),
  );

  if (!diagram) return <></>;
  return <Renderer diagram={diagram} />;
}
