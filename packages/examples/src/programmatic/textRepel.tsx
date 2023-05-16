// import {
//   dist,
//   eq,
//   measureText,
//   onCanvasPoint,
//   onCanvasRect,
//   problem,
//   scalar,
//   sdfRect,
//   textBBox,
// } from "@penrose/core";

// export default async function (
//   rng: seedrandom.prng,
//   canvas: [number, number],
//   names: string[]
// ) {
//   const [w, h] = canvas;
//   const n = names.length;

//   const points = names.map(() => [scalar(rng() * w), scalar(rng() * h)]);
//   await problem(
//     0,
//     names.flatMap((_, i) => {
//       const p = points[i];
//       const q = points[(i + 1) % n];
//       return [onCanvasPoint(canvas, p), eq(dist(p, q), 100)];
//     })
//   ).then((p) => p.minimize);

//   const text = names.map((name) => {
//     const [x, y] = [scalar(rng() * w), scalar(rng() * h)];
//     return {
//       x,
//       y,
//       rect: textBBox(
//         measureText(name, `font: 1.2em "Fira Sans", sans-serif;`),
//         x,
//         y
//       ),
//     };
//   });
//   problem(
//     0,
//     text.flatMap(({ rect }, i) => {
//       const [x, y] = points[i];
//       return [
//         onCanvasRect(canvas, rect),
//         eq(sdfRect(rect.center, rect.width, rect.height, [x.val, y.val]), 10),
//       ];
//     })
//   ).then((p) => p.minimize);

//   return (
//     <svg width={w} height={h}>
//       <polygon points={points.map(([x, y]) => `${x.val},${y.val}`).join(" ")} />
//       {names.map((name, i) => {
//         const { x, y } = text[i];
//         return (
//           <text x={x.val} y={y.val}>
//             {name}
//           </text>
//         );
//       })}
//     </svg>
//   );
// }
