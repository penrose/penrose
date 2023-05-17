import {
  dist,
  eq,
  measureText,
  onCanvasPoint,
  onCanvasRect,
  problem,
  scalar,
  sdfRect,
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
  await problem(
    0,
    names.flatMap((_, i) => {
      const p = points[i];
      const q = points[(i + 1) % n];
      return [onCanvasPoint(p, canvas), eq(dist(p, q), 100)];
    })
  ).then((p) => p.minimize());

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
        eq(sdfRect(rect.center, rect.width, rect.height, [x.val, y.val]), 10),
      ];
    })
  ).then((p) => p.minimize());

  const font = `15px "Courier"`;
  return (
    <svg version="1.2" xmlns="http://www.ws.org/2000/svg" width={w} height={h}>
      <polygon
        fill="#f0f7"
        points={points.map(([x, y]) => `${x.val},${y.val}`).join(" ")}
      />
      {names.map((name, i) => {
        const { x, y } = text[i];
        const measure = measureText(name, font);
        return (
          <g>
            <text
              x={x.val - measure.width / 2}
              y={y.val + measure.height / 2}
              fontSize={"15px"}
              fontFamily={"Courier"}
            >
              {name}
            </text>
            <circle r={1} fill="#f00" cx={x.val} cy={y.val} />
            <rect
              fill="#f007"
              x={x.val - measure.width / 2}
              y={y.val + measure.height / 2 - measure.actualAscent}
              width={measure.width}
              height={measure.height}
            />
          </g>
        );
      })}
    </svg>
  );
}
