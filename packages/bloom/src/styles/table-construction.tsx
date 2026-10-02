/** @jsxImportSource @penrose/bloom */
import { add, div, max, mul } from "@penrose/core";
import type {
  DiagramBuilder,
  InteractiveLayoutOptions,
} from "../core/builder.js";
import type { Equation, Line, Rectangle } from "../core/types.js";

/** One native hit region moves the entire table, keeping every incidence fixed. */
export function draggableTableConstruction(
  builder: DiagramBuilder,
  shapes: readonly (Equation | Line)[],
  name: string,
  options: InteractiveLayoutOptions,
) {
  const abs = (n: Equation["width"]) => max(n, mul(-1, n));
  let halfWidth = shapes.reduce(
    (width, shape) => {
      if ("center" in shape)
        return max(width, add(abs(shape.center[0]), div(shape.width, 2)));
      return max(width, max(abs(shape.start[0]), abs(shape.end[0])));
    },
    0 as Equation["width"],
  );
  let halfHeight = shapes.reduce(
    (height, shape) => {
      if ("center" in shape)
        return max(height, add(abs(shape.center[1]), div(shape.height, 2)));
      return max(height, max(abs(shape.start[1]), abs(shape.end[1])));
    },
    0 as Equation["height"],
  );
  halfWidth = add(halfWidth, 4);
  halfHeight = add(halfHeight, 4);
  const handle = (
    <rect
      name={name}
      center={[0, 0]}
      width={mul(halfWidth, 2)}
      height={mul(halfHeight, 2)}
      fill-color={[0, 0, 0, 0]}
      stroke-width={0}
      pointer-events="all"
      aria-label="Move the whole mathematical table"
    />
  ) as Rectangle;
  // The source tables have sufficient canvas margins for at most eight pixels.
  const maxDistance = Math.min(options.maxDistance ?? 8, 8);
  builder.draggableGroup(handle, shapes, {
    ...options,
    maxDistance,
    jitter: Math.min(options.jitter ?? 2, maxDistance),
  });
}
