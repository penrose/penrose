/** @jsxImportSource @penrose/bloom */

import type { Polygon } from "../core/types.js";
import {
  coordinateEmbed,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { hatchedTopologyPolygon } from "./topology-bases.js";

type XY = [number, number];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];

/** A shared product style handles a projection strip and the intersection of two strips. */
export function productNeighborhoodStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 84;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Product scale must be finite and positive");
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "18px",
    });
    const openProducts = ctx.entities(topology.OpenProductSet);
    const products = ctx
      .facts(topology.ProductOf)
      .filter(([p]) => openProducts.some((open) => open === p));
    const intervals = ctx.entities(topology.OpenInterval);
    const reals = ctx.entities(topology.RealLine);
    const regionView = ctx.view(topology.OpenProductSet, () => ({
      region: undefined as Polygon | undefined,
    }));
    const descriptions = products.map(([product, first, second]) => {
      const entity = openProducts.find((open) => open === product)!;
      const x = intervals.find((i) => i === first),
        y = intervals.find((i) => i === second);
      if (
        (!x && !reals.some((r) => r === first)) ||
        (!y && !reals.some((r) => r === second)) ||
        (!x && !y)
      )
        throw new Error(
          "This product style uses real-line and open-interval factors",
        );
      for (const interval of [x, y])
        if (
          interval &&
          (!(interval.a < interval.b) ||
            ![interval.a, interval.b].every(Number.isFinite))
        )
          throw new Error("Product endpoints must increase");
      return { entity, x, y };
    });
    const rectangle = descriptions.find(({ x, y }) => x && y);
    const isIntersection = Boolean(rectangle);
    if (descriptions.length !== (isIntersection ? 3 : 1))
      throw new Error("Expected one strip or a two-strip product intersection");
    const strips = descriptions.filter(({ x, y }) => !x || !y);
    if (isIntersection) {
      const family = ctx
        .facts(topology.FiniteIntersectionOf)
        .find(([p]) => p === rectangle!.entity)?.[1];
      if (
        !family ||
        strips.length !== 2 ||
        !strips.every(
          ({ entity }) =>
            ctx.test(topology.SetInFamily, entity, family) &&
            ctx.test(topology.Subset, rectangle!.entity, entity),
        ) ||
        !strips.some(({ x, y }) => x === rectangle!.x && !y) ||
        !strips.some(({ x, y }) => !x && y === rectangle!.y)
      )
        throw new Error(
          "The product rectangle must intersect its two coordinate strips",
        );
    }
    const origin: XY = isIntersection ? [-87, -64] : [-130, 0];
    const window: [number, number, number, number] = isIntersection
      ? [origin[0] - 96, origin[0] + 265, origin[1] - 66, origin[1] + 214]
      : [origin[0] - 50, origin[0] + 300, -86, 86];
    const physical = (x: number, y: number): XY => [
      origin[0] + x * unit,
      origin[1] + y * unit,
    ];
    const box = (bounds: [number, number, number, number]): XY[] => {
      const [left, right, bottom, top] = bounds;
      const corners: XY[] = [
        [left, bottom],
        [right, bottom],
        [right, top],
        [left, top],
      ];
      return corners.map(draw.xy);
    };
    for (const { entity, x, y } of strips) {
      const bounds: [number, number, number, number] = [
        x ? physical(x.a, 0)[0] : window[0],
        x ? physical(x.b, 0)[0] : window[1],
        y ? physical(0, y.a)[1] : window[2],
        y ? physical(0, y.b)[1] : window[3],
      ];
      regionView.get(entity).region = hatchedTopologyPolygon(
        x ? "product.vertical-strip" : "product.horizontal-strip",
        box(bounds),
        x && isIntersection ? -Math.PI / 4 : Math.PI / 4,
        options.regionColor,
      );
      const interval = x ?? y!;
      const projection = ctx
        .entities(topology.CoordinateProjection)
        .find(
          (p) =>
            p.coordinate === (x ? 1 : 2) &&
            ctx.test(topology.InverseImageOf, entity, p, interval),
        );
      if (!projection)
        throw new Error("A coordinate strip must be an interval inverse image");
      const endpoints = ctx
        .entities(topology.CoordinatePoint)
        .filter((p) => ctx.test(topology.BoundaryPoint, p, entity));
      if (endpoints.length !== 2)
        throw new Error(
          "Each displayed strip requires its two boundary intercepts",
        );
      for (const point of endpoints) {
        const coordinate = point.coordinates[x ? 0 : 1];
        if (
          (coordinate !== interval.a && coordinate !== interval.b) ||
          point.coordinates[x ? 1 : 0] !== 0 ||
          !ctx.test(topology.Outside, point, entity)
        )
          throw new Error("An open strip's boundary intercepts are outside it");
        const at = physical(point.coordinates[0], point.coordinates[1]);
        <circle
          name={`${entity.label}.${coordinate}`}
          center={draw.xy(at)}
          r={2.8 * draw.scale}
          fill-color={INK}
          stroke-width={0}
          aria-label={`${entity.label} boundary intercept`}
          data-included="false"
        />;
        const opening = coordinate === interval.a;
        if (isIntersection)
          <equation
            center={draw.xy(at)}
            font-size="26px"
            fill-color={INK}
            aria-label={`${entity.label} ${
              opening ? "lower" : "upper"
            } endpoint`}
            data-included="false"
          >
            {x ? (opening ? "(" : ")") : opening ? "\\smile" : "\\frown"}
          </equation>;
        draw.label(
          point.label,
          x
            ? [
                at[0] + (opening ? -24 : 26),
                at[1] + (isIntersection ? 13 : -13),
              ]
            : [at[0] + 26, at[1] + (opening ? -12 : 38)],
        );
      }
      if (!isIntersection) {
        <rect
          center={draw.xy([(bounds[0] + bounds[1]) / 2, 33])}
          width={84}
          height={23}
          fill-color={[1, 1, 1, 1]}
          stroke-width={0}
        />;
        draw.label(entity.label, [(bounds[0] + bounds[1]) / 2, 33]);
      } else if (x)
        draw.label(entity.label, [(bounds[0] + bounds[1]) / 2, window[2] - 18]);
      else draw.label(entity.label, [origin[0] - 45, bounds[3] + 18]);
    }
    if (rectangle) {
      const x = rectangle.x!,
        y = rectangle.y!;
      regionView.get(rectangle.entity).region = (
        <polygon
          name="product.rectangle"
          points={box([
            physical(x.a, 0)[0],
            physical(x.b, 0)[0],
            physical(0, y.a)[1],
            physical(0, y.b)[1],
          ])}
          fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.08]}
          stroke-width={0}
          aria-label="product.rectangle"
        />
      ) as Polygon;
      draw.label(rectangle.entity.label, [
        physical((x.a + x.b) / 2, 0)[0] - 82,
        physical(0, y.b)[1] + 18,
      ]);
    }
    draw.line("product.axis-x", [window[0], origin[1]], [window[1], origin[1]]);
    draw.line("product.axis-y", [origin[0], window[2]], [origin[0], window[3]]);
    if (!isIntersection) {
      draw.label("x", [window[1] + 7, origin[1] + 3]);
      draw.label("R", [window[1] - 9, origin[1] - 13]);
      draw.label("y", [origin[0], window[3] + 6]);
      draw.label("R", [origin[0] + 10, window[3] - 14]);
    }
  });
}

/** Coordinate inclusion draws its image line and the ambient transverse axis. */
export function coordinateEmbeddingStyle(
  options: TopologyStyleOptions & {
    unit?: number;
    /** Show the ambient coordinate axes separately from a highlighted image slice. */
    showAmbientAxes?: boolean;
  } = {},
) {
  const unit = options.unit ?? 84;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Embedding scale must be finite and positive");
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "17px",
    });
    const embeddings = ctx.entities(topology.CoordinateEmbedding);
    if (embeddings.length !== 1)
      throw new Error("The coordinate inclusion panel requires one embedding");
    const map = embeddings[0];
    const fact = ctx.facts(topology.EmbeddingInto).find(([f]) => f === map);
    if (
      !fact ||
      !ctx.test(topology.ImageOf, fact[3], map, fact[1]) ||
      !ctx.facts(topology.SubspaceTopologyOf).some(([, s]) => s === fact[3]) ||
      !ctx
        .facts(topology.CorestrictionOf)
        .some(([, f, s]) => f === map && s === fact[3])
    )
      throw new Error(
        "The embedding must identify its image and the induced subspace topology",
      );
    const origin: XY = [-14, 0];
    const rotate = ([x, y]: XY): XY =>
      map.varyingCoordinate === 1 ? [x + origin[0], y] : [y + origin[0], x];
    const lineOffset = map.fixedCoordinate * unit;
    if (options.showAmbientAxes) {
      draw.line(
        "embedding.ambient-coordinate-axis",
        rotate([-138, 0]),
        rotate([146, 0]),
        true,
      );
      <line
        start={draw.xy(rotate([-138, lineOffset]))}
        end={draw.xy(rotate([146, lineOffset]))}
        stroke-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.3]}
        stroke-width={7}
        aria-label="embedding.highlighted-image-slice"
      />;
      draw.label(
        fact[3].label,
        rotate([124, lineOffset + (map.varyingCoordinate === 1 ? 46 : 70)]),
      );
      draw.label(String(map.fixedCoordinate), rotate([-18, lineOffset + 24]));
    }
    draw.line(
      "embedding.image-line",
      rotate([-138, lineOffset]),
      rotate([146, lineOffset]),
    );
    draw.line(
      "embedding.transverse-axis",
      rotate([0, -87]),
      rotate([0, 87]),
      true,
    );
    draw.label(
      map.varyingCoordinate === 1 ? "x" : "y",
      rotate([156, options.showAmbientAxes ? 0 : lineOffset]),
    );
    draw.label(map.varyingCoordinate === 1 ? "y" : "x", rotate([0, 95]));
    const mappings = ctx.facts(topology.MapsTo).filter(([f]) => f === map);
    if (mappings.length !== 1)
      throw new Error(
        "The diagram requires one representative mapped real point",
      );
    const [, source, target] = mappings[0];
    const real = ctx.entities(topology.RealPoint).find((p) => p === source);
    const point = ctx
      .entities(topology.CoordinatePoint)
      .find((p) => p === target);
    if (
      !real ||
      !point ||
      coordinateEmbed(map, real.coordinate).some(
        (c, i) => c !== point.coordinates[i],
      ) ||
      !ctx.test(topology.Member, point, fact[3])
    )
      throw new Error(
        "The mapped point must lie on the coordinate embedding's image",
      );
    const at: XY = [
      point.coordinates[0] * unit + origin[0],
      point.coordinates[1] * unit,
    ];
    if (options.showAmbientAxes)
      draw.line(
        "embedding.fixed-coordinate-guide",
        at,
        map.varyingCoordinate === 1 ? [at[0], 0] : [origin[0], at[1]],
        true,
      );
    draw.dot("embedding.image-point", at);
    const label = `${real.label}\\mapsto${point.label}`;
    draw.label(label, [
      at[0] + (options.showAmbientAxes && map.varyingCoordinate === 2 ? 68 : 0),
      at[1] + 16,
    ]);
  });
}
