/** @jsxImportSource @penrose/bloom */

import type { PathData, Polygon } from "../core/types.js";

export type SurfaceVector = readonly [number, number, number];
export interface SurfaceSample {
  position: SurfaceVector;
  /** Outward unit normal. */
  normal: SurfaceVector;
}
export type ParametricSurface = (u: number, v: number) => SurfaceSample;
export interface SurfaceViewOptions {
  uSteps?: number;
  vSteps?: number;
  /** Elevation of the orthographic camera above the xy plane, in radians. */
  elevation?: number;
  offset?: readonly [number, number];
  color?: readonly [number, number, number];
}

/** A geometric realization of a product of circles; radii belong to its view. */
export function torusSurface(
  majorRadius: number,
  minorRadius: number,
): ParametricSurface {
  if (
    !(majorRadius > minorRadius && minorRadius > 0) ||
    ![majorRadius, minorRadius].every(Number.isFinite)
  )
    throw new Error("A ring torus needs finite radii R > r > 0");
  return (u, v) => {
    const cu = Math.cos(u),
      su = Math.sin(u),
      cv = Math.cos(v),
      sv = Math.sin(v);
    return {
      position: [
        (majorRadius + minorRadius * cv) * cu,
        (majorRadius + minorRadius * cv) * su,
        minorRadius * sv,
      ],
      normal: [cv * cu, cv * su, sv],
    };
  };
}

export function surfaceProjection(
  elevation: number,
  offset: readonly [number, number] = [0, 0],
) {
  if (!Number.isFinite(elevation) || !offset.every(Number.isFinite))
    throw new Error("Surface view must be finite");
  const s = Math.sin(elevation),
    c = Math.cos(elevation);
  return {
    point: ([x, y, z]: SurfaceVector): [number, number] => [
      x + offset[0],
      y * s + z * c + offset[1],
    ],
    depth: ([, y, z]: SurfaceVector) => y * c - z * s,
    facing: ([, ny, nz]: SurfaceVector) => -ny * c + nz * s,
  };
}

interface ProjectedVertex {
  point: [number, number];
  depth: number;
  facing: number;
  grid: [number, number];
}
interface SurfacePatch {
  vertices: ProjectedVertex[];
  depth: number;
  shade: number;
}

function triangleDepth(
  p: [number, number],
  a: ProjectedVertex,
  b: ProjectedVertex,
  c: ProjectedVertex,
) {
  const [x, y] = p,
    [ax, ay] = a.point,
    [bx, by] = b.point,
    [cx, cy] = c.point;
  const denominator = (by - cy) * (ax - cx) + (cx - bx) * (ay - cy);
  if (Math.abs(denominator) < 1e-9) return undefined;
  const wa = ((by - cy) * (x - cx) + (cx - bx) * (y - cy)) / denominator;
  const wb = ((cy - ay) * (x - cx) + (ax - cx) * (y - cy)) / denominator;
  const wc = 1 - wa - wb;
  if (Math.min(wa, wb, wc) < -1e-7) return undefined;
  return wa * a.depth + wb * b.depth + wc * c.depth;
}

/** Native Penrose polygons with depth ordering, restrained diffuse shading and visible contours. */
export function parametricSurfaceView(
  name: string,
  surface: ParametricSurface,
  options: SurfaceViewOptions = {},
) {
  const nu = options.uSteps ?? 64,
    nv = options.vSteps ?? 24;
  if (![nu, nv].every((n) => Number.isInteger(n) && n >= 8 && n <= 128))
    throw new Error("Surface resolution must be an integer from 8 to 128");
  const project = surfaceProjection(options.elevation ?? 0.46, options.offset);
  const base = options.color ?? [0.95, 0.41, 0.12];
  const sample = (u: number, v: number) =>
    surface((2 * Math.PI * u) / nu, (2 * Math.PI * v) / nv);
  const vertices = Array.from({ length: nu }, (_, i) =>
    Array.from({ length: nv }, (_, j): ProjectedVertex => {
      const p = sample(i, j);
      if (![...p.position, ...p.normal].every(Number.isFinite))
        throw new Error("Surface samples must be finite");
      return {
        point: project.point(p.position),
        depth: project.depth(p.position),
        facing: project.facing(p.normal),
        grid: [i, j],
      };
    }),
  );
  const patches: SurfacePatch[] = [];
  for (let i = 0; i < nu; i++)
    for (let j = 0; j < nv; j++) {
      const corners = [
        vertices[i][j],
        vertices[(i + 1) % nu][j],
        vertices[(i + 1) % nu][(j + 1) % nv],
        vertices[i][(j + 1) % nv],
      ];
      const normal = sample(i + 0.5, j + 0.5).normal;
      // Light arrives from above and to the left, independent of the camera.
      const diffuse = Math.max(
        0,
        -0.35 * normal[0] - 0.45 * normal[1] + 0.82 * normal[2],
      );
      patches.push({
        vertices: corners,
        depth: corners.reduce((sum, p) => sum + p.depth, 0) / 4,
        shade: 0.22 + 0.7 * diffuse,
      });
    }
  const polygons: Polygon[] = [];
  for (const patch of [...patches].sort((a, b) => b.depth - a.depth)) {
    const color: [number, number, number, number] = [
      base[0] + (1 - base[0]) * patch.shade,
      base[1] + (1 - base[1]) * patch.shade,
      base[2] + (1 - base[2]) * patch.shade,
      1,
    ];
    polygons.push(
      (
        <polygon
          points={patch.vertices.map((v) => v.point)}
          fill-color={color}
          stroke-color={color}
          stroke-width={0.25}
          ensure-on-canvas={false}
        />
      ) as Polygon,
    );
  }
  <g
    name={`${name}.surface`}
    aria-label={`${name} shaded surface`}
    ensure-on-canvas={false}
  >
    {polygons}
  </g>;
  const contours: PathData = [];
  const visible = (p: [number, number], depth: number) =>
    !patches.some((patch) => {
      const [a, b, c, d] = patch.vertices;
      const front = triangleDepth(p, a, b, c) ?? triangleDepth(p, a, c, d);
      return front !== undefined && front < depth - 1.6;
    });
  // Marching squares locates normal/camera sign changes on each parameter patch.
  for (const patch of patches) {
    const crossing: ProjectedVertex[] = [];
    for (let k = 0; k < 4; k++) {
      const a = patch.vertices[k],
        b = patch.vertices[(k + 1) % 4];
      if (a.facing >= 0 === b.facing >= 0) continue;
      // Locate the contour on the surface itself. Linear interpolation of the
      // projected mesh would put silhouette points inside their tangent rays.
      let du = b.grid[0] - a.grid[0],
        dv = b.grid[1] - a.grid[1];
      if (du > nu / 2) du -= nu;
      if (du < -nu / 2) du += nu;
      if (dv > nv / 2) dv -= nv;
      if (dv < -nv / 2) dv += nv;
      let low = 0,
        high = 1;
      for (let step = 0; step < 12; step++) {
        const mid = (low + high) / 2;
        const f = project.facing(
          sample(a.grid[0] + du * mid, a.grid[1] + dv * mid).normal,
        );
        if (f >= 0 === a.facing >= 0) low = mid;
        else high = mid;
      }
      const t = (low + high) / 2;
      const grid: [number, number] = [a.grid[0] + du * t, a.grid[1] + dv * t];
      const located = sample(...grid);
      crossing.push({
        point: project.point(located.position),
        depth: project.depth(located.position),
        facing: 0,
        grid,
      });
    }
    if (crossing.length !== 2) continue;
    const [a, b] = crossing;
    const mid: [number, number] = [
      (a.point[0] + b.point[0]) / 2,
      (a.point[1] + b.point[1]) / 2,
    ];
    if (!visible(mid, (a.depth + b.depth) / 2)) continue;
    contours.push(
      { cmd: "M", contents: [{ tag: "CoordV", contents: a.point }] },
      { cmd: "L", contents: [{ tag: "CoordV", contents: b.point }] },
    );
  }
  <path
    name={`${name}.silhouette`}
    d={contours}
    fill-color={[0, 0, 0, 0]}
    stroke-color={[0.08, 0.08, 0.08, 1]}
    stroke-width={1.05}
  />;
  return { project, surface, patches: patches.length };
}
