import {
  dist,
  eq,
  measureText,
  onCanvasPoint,
  onCanvasRect,
  problem,
  sdfRect,
  textBBox,
  variable,
} from "@penrose/core";
import seedrandom from "seedrandom";
import { JSX } from "solid-js/jsx-runtime";

export default async function Text(
  rng: seedrandom.prng,
  canvas: [number, number],
  names: string[]
): Promise<JSX.Element> {
  const [w, h] = canvas;
  const n = names.length;

  const points = names.map(() => [variable(rng() * w), variable(rng() * h)]);
  await problem(
    0,
    names.flatMap((_, i) => {
      const p = points[i];
      const q = points[(i + 1) % n];
      return [onCanvasPoint(p, canvas), eq(dist(p, q), 100)];
    })
  ).then((p) => p.minimize());

  const texts = names.map((name) => {
    const [x, y] = [variable(rng() * w), variable(rng() * h)];
    return {
      name,
      x,
      y,
      rect: textBBox(
        measureText(name, `font: 1.2em "Fira Sans", sans-serif;`),
        x,
        y
      ),
    };
  });
  (
    await problem(
      0,
      texts.flatMap(({ rect }, i) => {
        const [x, y] = points[i];
        return [
          onCanvasRect(canvas, rect),
          eq(sdfRect(rect.center, rect.width, rect.height, [x.val, y.val]), 10),
        ];
      })
    )
  ).minimize();

  const font = `15px "Courier"`;
  return (
    <svg version="1.2" xmlns="http://www.w3.org/2000/svg" width={w} height={h}>
      <polygon
        fill="#f0f7"
        points={points.map(([x, y]) => `${x.val},${y.val}`).join(" ")}
      />
      {texts.map(({ x, y, name }, i) => {
        const measure = measureText(name, font);
        return (
          <g>
            <text
              x={x.val - measure.width / 2}
              y={y.val + measure.height / 2}
              font-size={"15px"}
              font-family={"Courier"}
            >
              {name}
            </text>
          </g>
        );
      })}
    </svg>
  );
}
