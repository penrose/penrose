/** @jsxImportSource @penrose/bloom */

import type { Circle, Line, Path } from "../core/types.js";
import { topologyDrawing } from "./point-set-topology.js";

/** A second source hatch direction, clipped by a native analytic contour. */
export function reverseRegionHatches(
  draw: ReturnType<typeof topologyDrawing>,
  mask: Circle | Path,
  bounds: readonly [number, number, number, number],
  name: string,
) {
  const [x, y, width, height] = bounds;
  const lines: Line[] = [];
  for (let offset = -height; offset < width; offset += 4.5) {
    lines.push(
      (
        <line
          start={draw.xy([x + offset, y + height])}
          end={draw.xy([x + offset + height, y])}
          stroke-color={[0.12, 0.12, 0.12, 0.6]}
          stroke-width={0.55}
        />
      ) as Line,
    );
  }
  return (
    <g name={name} clip-path={mask}>
      {lines}
    </g>
  );
}
