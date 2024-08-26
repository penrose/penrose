import {
  DiagramBuilder,
  Renderer,
  SharedInput,
  canvas,
  constraints,
  useDiagram,
  useSharedInput,
} from "@penrose/bloom";
import { ops, sub } from "@penrose/core";
import { useCallback } from "react";

const circlePackingDiagram = async (
  inputs: {
    containerRad: SharedInput;
    circleRad: SharedInput;
  },
  constraintType: "disjoint" | "disjoint-padded" | "equally-spaced",
) => {
  const width = 400;
  const height = 400;
  const numCircles = 15;

  const { circle, ensure, type, forall, build, sharedInput, encourage } =
    new DiagramBuilder(canvas(width, height), "", 1e3);

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
    let dragConstraint;
    if (constraintType === "equally-spaced") {
      dragConstraint = ([x, y]: [number, number]): [number, number] => {
        const norm = Math.sqrt(x * x + y * y);
        const targetNorm =
          inputs.containerRad.get()! -
          // containerBorder / 2 -
          inputs.circleRad.get()!;
        return [(x / norm) * targetNorm, (y / norm) * targetNorm];
      };
    } else {
      dragConstraint = ([x, y]: [number, number]): [number, number] => {
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
      };
    }
    c.icon = circle({
      r: circleRad,
      drag: true,
      dragConstraint,
    });

    c.icon.fillColor[3] = 1;

    ensure(constraints.contains(enclosure.icon, c.icon, containerBorder / 2));
  });

  forall({ c1: Circle, c2: Circle }, ({ c1, c2 }, i) => {
    console.log(constraintType);
    switch (constraintType) {
      case "disjoint":
        ensure(constraints.disjoint(c1.icon, c2.icon));
        break;
      case "disjoint-padded":
        ensure(constraints.disjoint(c1.icon, c2.icon, 20));
        break;
      case "equally-spaced":
        ensure(constraints.disjoint(c1.icon, c2.icon));
    }
  });

  if (constraintType === "equally-spaced") {
    forall({ c: Circle }, ({ c }) => {
      ensure(
        constraints.equal(
          ops.vnorm(c.icon.center),
          sub(enclosure.icon.r, circleRad),
        ),
      );
    });
  }

  return await build();
};

export default function CirclePackingDiagram(props: {
  constraintType: "disjoint" | "disjoint-padded" | "equally-spaced";
}) {
  const defaultContRad = 150;
  const defaultCircRad = 20;
  const containerRad = useSharedInput(defaultContRad);
  const circleRad = useSharedInput(defaultCircRad);

  const diagram = useDiagram(
    useCallback(
      () =>
        circlePackingDiagram({ containerRad, circleRad }, props.constraintType),
      [containerRad, circleRad],
    ),
  );

  return <Renderer diagram={diagram} />;
}
