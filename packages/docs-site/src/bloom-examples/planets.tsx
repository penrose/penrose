import {
  DiagramBuilder,
  Renderer,
  canvas,
  cos,
  div,
  mul,
  sin,
  useDiagram,
} from "@penrose/bloom";
import { useCallback } from "react";

const buildDiagram = async (arrows: boolean) => {
  const db = new DiagramBuilder(canvas(500, 500), "");
  const { build, circle, type, forall, input, bindToInput, line } = db;

  const Sun = type();
  const Planet = type();
  const ArrowPair = type();

  const sun = Sun();

  if (!arrows) {
    const mercury = Planet();
    mercury.period = 88;
    mercury.orbitalRad = 58;
    mercury.rad = 6;
    //brown
    mercury.color = [0.5, 0.3, 0.1, 1];

    const venus = Planet();
    venus.period = 225;
    venus.orbitalRad = 107;
    venus.rad = 13.5;
    venus.color = [0.9, 0.7, 0.5, 1];

    const mars = Planet();
    mars.period = 687;
    mars.orbitalRad = 228;
    mars.rad = 7.5;
    // rust color
    mars.color = [0.8, 0.3, 0, 1];
  }

  const earth = Planet();
  earth.period = 365;
  earth.orbitalRad = 150;
  earth.rad = 15;
  //dark green
  earth.color = [0, 0.5, 0, 1];

  if (arrows) {
    const pairs = 8;
    for (let i = 0; i < pairs; ++i) {
      const angle = ((2 * Math.PI) / pairs) * i;

      const arrow1 = ArrowPair();
      // pointing from sun to earth orbit, padded from both
      arrow1.start = [100 * Math.cos(angle), 100 * Math.sin(angle)];
      arrow1.end = [
        0.9 * earth.orbitalRad * Math.cos(angle),
        0.9 * earth.orbitalRad * Math.sin(angle),
      ];

      const arrow2 = ArrowPair();
      arrow2.start = [200 * Math.cos(angle), 200 * Math.sin(angle)];
      arrow2.end = [
        1.1 * earth.orbitalRad * Math.cos(angle),
        1.1 * earth.orbitalRad * Math.sin(angle),
      ];
    }
  }

  sun.icon = circle({
    r: 30,
    // orange yellow
    fillColor: [1, 0.8, 0, 1],
    center: [0, 0],
  });

  const time = input();

  forall({ p: Planet }, ({ p }) => {
    p.orbit = circle({
      r: p.orbitalRad,
      strokeColor: [0, 0, 0, 1],
      strokeWidth: 1,
      fillColor: [0, 0, 0, 0],
      center: [0, 0],
    });

    const center = [
      mul(p.orbitalRad, cos(div(time, p.period))),
      mul(p.orbitalRad, sin(div(time, p.period))),
    ];

    p.icon = circle({
      r: p.rad,
      fillColor: p.color,
      center: [bindToInput(center[0]), bindToInput(center[1])],
      drag: true,
      dragConstraint: ([x, y]) => {
        const norm = Math.sqrt(x ** 2 + y ** 2);
        const targetNorm = p.orbitalRad;
        return [(x * targetNorm) / norm, (y * targetNorm) / norm];
      },
    });
  });

  forall({ p: ArrowPair }, ({ p }) => {
    p.arrow = line({
      start: p.start,
      end: p.end,
      endArrowhead: "straight",
    });
  });

  return await build();
};

export default function Planets(props: { arrows: boolean }) {
  const diagram = useDiagram(
    useCallback(() => buildDiagram(props.arrows), [props.arrows]),
  );
  return <Renderer diagram={diagram} />;
}
