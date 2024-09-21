import {
  DiagramBuilder,
  Renderer,
  Shape,
  SharedInput,
  canvas,
  normalize,
  rayIntersect,
  useDiagram,
} from "@penrose/bloom";
import { hexToRgba, ops } from "@penrose/core";
import { useCallback, useState } from "react";

const raysDiagram = async (
  squareCenters: [SharedInput, SharedInput][],
  trianglePoints: [
    [SharedInput, SharedInput],
    [SharedInput, SharedInput],
    [SharedInput, SharedInput],
  ][],
) => {
  const width = 800;
  const height = 700;
  const {
    rectangle,
    circle,
    group,
    build,
    input,
    layer,
    type,
    forall,
    line,
    polygon,
    ensure,
    sharedInput,
  } = new DiagramBuilder(canvas(width, height), "");

  const Ray = type();

  const Square = type();
  const Triangle = type();
  const Source = type();
  const Ground = type();

  const numRays = 50;
  for (let i = 0; i < numRays; ++i) {
    Ray();
  }

  for (const [x, y] of squareCenters) {
    const s = Square();
    s.center = [sharedInput(x), sharedInput(y)];
  }

  for (const [[x1, y1], [x2, y2], [x3, y3]] of trianglePoints) {
    const t = Triangle();
    t.A = [sharedInput(x1), sharedInput(y1)];
    t.B = [sharedInput(x2), sharedInput(y2)];
    t.C = [sharedInput(x3), sharedInput(y3)];
  }

  const source = Source();
  const ground = Ground();

  const shapes: Shape[] = [];

  forall({ g: Ground }, ({ g }) => {
    g.level = -height / 2 + 25;
    g.icon = line({
      start: [-width / 2 + 10, g.level],
      end: [width / 2 - 10, g.level],
      strokeWidth: 3,
      strokeColor: [0, 0, 0, 0.5],
    });

    shapes.push(g.icon);
  });

  forall({ s: Source }, ({ s }) => {
    s.icon = circle({
      center: [0, height / 2 - 25],
      r: 3,
      fillColor: [0, 0, 0, 1],
    });
  });

  forall({ t: Triangle }, ({ t }) => {
    const sideLen = 100;

    t.icon = polygon({
      points: [t.A, t.B, t.C],
      drag: true,
    });

    shapes.push(t.icon);
  });

  forall({ s: Square }, ({ s }) => {
    s.icon = rectangle({
      center: s.center,
      width: 100,
      height: 100,
      drag: true,
    });

    shapes.push(s.icon);
  });

  const scene = group({
    shapes,
  });

  forall({ r: Ray }, ({ r }, i) => {
    const extendedToGround = [
      ground.icon.start[0] +
        ((ground.icon.end[0] - ground.icon.start[0]) * i) / (numRays - 1),
      ground.level,
    ];
    r.vec = normalize(ops.vsub(extendedToGround, source.icon.center));

    r.end = rayIntersect(scene, source.icon.center, r.vec);

    r.endIcon = circle({
      center: r.end,
      r: 2.5,
      strokeWidth: 1.5,
      strokeColor: [0, 0, 0, 1],
      fillColor: hexToRgba("ffff"),
    });
    r.line = line({
      start: source.icon.center,
      end: r.end,
      strokeColor: [1, 0.5, 0, 0.1],
      strokeWidth: 3.5,
      ensureOnCanvas: false,
    });

    layer(r.line, source.icon);
  });

  return await build();
};

export default function RaysComponent() {
  const [squareCenters, setSquareCenters] = useState<
    [SharedInput, SharedInput][]
  >([]);
  const [trianglePoints, setTrianglePoints] = useState<
    [
      [SharedInput, SharedInput],
      [SharedInput, SharedInput],
      [SharedInput, SharedInput],
    ][]
  >([]);

  const diagram = useDiagram(
    useCallback(async () => {
      return raysDiagram(squareCenters, trianglePoints);
    }, [squareCenters, trianglePoints]),
  );

  return (
    <div
      style={{
        marginTop: "1em",
        height: "100%,",
        display: "flex",
        alignItems: "center",
        flexDirection: "column",
      }}
    >
      <button
        style={{
          backgroundColor:
            squareCenters.length >= 5 ? "rgb(113 116 120)" : "rgb(226 232 240)",
          padding: "5px 10px",
          borderRadius: "5px",
        }}
        className="bg-red"
        disabled={squareCenters.length >= 5}
        onClick={() => {
          const x = Math.random() * 300 - 150;
          const y = Math.random() * 300 - 150;
          setSquareCenters([
            ...squareCenters,
            [new SharedInput(x), new SharedInput(y)],
          ]);
        }}
      >
        Add Square
      </button>
      {squareCenters.length >= 5 && <p>Max squares: 5</p>}
      <Renderer diagram={diagram} />
    </div>
  );
}
