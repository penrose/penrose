/** @jsxImportSource @penrose/bloom */

import type { Diagram } from "@penrose/bloom";
import { DiagramBuilder, canvas, constraints } from "@penrose/bloom";

export const buildTwoTrianglesDiagram = async (): Promise<Diagram> => {
  const db = new DiagramBuilder(canvas(200, 200), "LightheartedCormorant8083");
  const { type, forall, ensure, build } = db;

  const Triangle = type();

  const s = Triangle();
  const t = Triangle();

  forall({ t: Triangle }, ({ t }) => {
    t.icon = (
      <polygon
        fill-color={[0.204, 0.216, 0.604, 0.667]}
        stroke-color={[0.106, 0.122, 0.541, 1]}
        stroke-width={0.5}
      />
    );
    t.labelI = <equation />;
    t.labelJ = <equation />;
    t.labelK = <equation />;
    t.shadow = <polygon fill-color={[0, 0, 0, 0.1]} />;
  });

  forall({ s: Triangle, t: Triangle }, ({ s, t }) => {
    ensure(constraints.disjoint(t.icon, s.icon));
  });

  return await build();
};
