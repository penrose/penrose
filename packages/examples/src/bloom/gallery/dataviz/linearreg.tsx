/** @jsxImportSource @penrose/bloom */
// Gallery translation of: dataviz/linearreg

import type { Diagram } from "@penrose/bloom";
import { canvas, DiagramBuilder } from "@penrose/bloom";

import { rgba } from "../utils.js";

const SEED = "PorcellanaFlamingo3169";
const scale = 25;
const startX = -7;
const endX = 7;

const points = [
  [3.09299794, 1.99154113],
  [1.61443451, 3.45205535],
  [2.71743126, 1.36957973],
  [-0.01918644, 2.00810678],
  [3.51099675, 4.65642173],
  [-0.07964473, 1.6433598],
  [-0.22104, 3.16761298],
  [-4.57959419, -0.31200357],
  [0.5923723, -0.69303941],
  [3.38924182, 2.74611905],
  [-6.34926085, 0.50522],
  [1.47127528, 0.89377317],
  [-3.95309602, 0.91984693],
  [1.33687037, 4.32999148],
  [6.3670986, 3.13758927],
  [-2.942611, 1.06663622],
  [-4.0381197, -0.66433243],
  [-1.46036612, 1.18601748],
  [-2.2642927, 1.6239366],
  [6.53381649, 3.78754777],
  [0.70013571, 3.76010956],
  [2.38176648, 2.50497593],
  [1.22763577, 2.44096362],
  [-2.72709625, 1.11249512],
  [1.90556844, 3.22617755],
  [-3.51381868, 2.10571365],
  [-0.5149938, 2.10662662],
  [-4.40975308, -0.60297607],
  [-5.622795, 1.39514354],
  [-0.82094253, -0.01695186],
] as const;

const mean = (xs: readonly number[]) =>
  xs.reduce((sum, x) => sum + x, 0) / xs.length;

const xs = points.map(([x]) => x);
const ys = points.map(([, y]) => y);
const xBar = mean(xs);
const yBar = mean(ys);
const beta =
  points.reduce((sum, [x, y]) => sum + (x - xBar) * (y - yBar), 0) /
  points.reduce((sum, [x]) => sum + (x - xBar) ** 2, 0);
const alpha = yBar - beta * xBar;

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const { build } = new DiagramBuilder(canvas(400, 400), variation);

  <line
    start={[-190, 0]}
    end={[190, 0]}
    end-arrowhead={"straight"}
    stroke-color={rgba(170, 170, 170, 1)}
  />;
  <line
    start={[0, -190]}
    end={[0, 190]}
    end-arrowhead={"straight"}
    stroke-color={rgba(170, 170, 170, 1)}
  />;

  for (const [x, y] of points) {
    <circle
      r={2}
      center={[x * scale, y * scale]}
      fill-color={rgba(0, 0, 0, 1)}
    />;
  }

  <line
    start={[scale * startX, scale * (alpha + beta * startX)]}
    end={[scale * endX, scale * (alpha + beta * endX)]}
    end-arrowhead={"straight"}
    stroke-color={rgba(255, 0, 0, 1)}
  />;

  return await build();
};

export default async (variation = SEED): Promise<string> => {
  const diagram = await buildDiagram(variation);
  const { svg } = await diagram.render();
  diagram.discard();
  return svg.outerHTML;
};
