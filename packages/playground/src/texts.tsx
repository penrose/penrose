import {
  dist,
  eq,
  measureText,
  onCanvasPoint,
  onCanvasRect,
  problem,
  scalar,
  textBBox,
} from "@penrose/core";
import seedrandom from "seedrandom";

export default async function Text(
  rng: seedrandom.prng,
  canvas: [number, number],
  names: string[]
) {
  const [w, h] = canvas;
  const n = names.length;

  const points = names.map(() => [scalar(rng() * w), scalar(rng() * h)]);
  const p = await problem(
    0,
    names.flatMap((_, i) => {
      const p = points[i];
      const q = points[(i + 1) % n];
      return [onCanvasPoint(p, canvas), eq(dist(p, q), 100)];
    })
  );
  p.minimize();

  const text = names.map((name) => {
    const [x, y] = [scalar(rng() * w), scalar(rng() * h)];
    return {
      x,
      y,
      rect: textBBox(
        measureText(name, `font: 1.2em "Fira Sans", sans-serif;`),
        x,
        y
      ),
    };
  });
  await problem(
    0,
    text.flatMap(({ rect }, i) => {
      const [x, y] = points[i];
      return [
        onCanvasRect(canvas, rect),
        // eq(sdfRect(rect.center, rect.width, rect.height, [x.val, y.val]), 10),
      ];
    })
  ).then((p) => p.minimize());

  return (
    <svg
      version="1.2"
      xmlns="http://www.ws.org/2000/svg"
      width={w}
      height={h}
      // viewBox={`0 0 ${w} ${h}`}
    >
      <polygon points={points.map(([x, y]) => `${x.val},${y.val}`).join(" ")} />
      {names.map((name, i) => {
        const { x, y } = text[i];
        const bbox = textBBox(
          measureText(name, `font: 1.2em "Fira Sans", sans-serif;`),
          x,
          y
        );
        return (
          <g>
            <text x={x.val} y={y.val}>
              {name}
            </text>
            <rect
              fill="#f007"
              x={bbox.center[0].val}
              y={bbox.center[1].val}
              width={bbox.width}
              height={bbox.height}
            />
          </g>
        );
      })}
    </svg>
  );
}
