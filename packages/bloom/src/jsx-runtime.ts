/**
 * JSX runtime for @penrose/bloom.
 *
 * Enables JSX syntax for creating Bloom shapes:
 *
 * ```tsx
 * &#47;** @jsxImportSource @penrose/bloom *&#47;
 * const { forall } = new DiagramBuilder(canvas(400, 400), "seed");
 * forall({ n: Node }, ({ n }) => {
 *   n.icon = <circle r={50} fill-color={[0, 0, 1, 1]} />;
 * });
 * ```
 *
 * Prop names use SVG-native kebab-case (e.g. `fill-color`, `stroke-width`).
 * They are converted to camelCase before being passed to the builder.
 */
import { getActiveBuilder } from "./core/builder.js";
import type { Num } from "@penrose/core";
import type {
  Color,
  DragConstraint,
  Group,
  PathData,
  Shape,
  Vec2,
} from "./core/types.js";

/** Convert an object with kebab-case keys to camelCase keys */
const kebabToCamel = (
  props: Record<string, unknown>,
): Record<string, unknown> => {
  const result: Record<string, unknown> = {};
  for (const [key, val] of Object.entries(props)) {
    const camelKey = key.replace(/-([a-z])/g, (_, c: string) =>
      c.toUpperCase(),
    );
    result[camelKey] = val;
  }
  return result;
};

/** Map from JSX intrinsic element names to DiagramBuilder method names */
const elementToMethod: Record<string, string> = {
  circle: "circle",
  ellipse: "ellipse",
  rect: "rectangle",
  line: "line",
  path: "path",
  polygon: "polygon",
  polyline: "polyline",
  text: "text",
  image: "image",
  g: "group",
  equation: "equation",
};

/** Sentinel value for JSX fragments (not supported — throws at runtime) */
export const Fragment: unique symbol = Symbol("Fragment");

/**
 * JSX factory function. Called by the TypeScript compiler for every JSX element.
 * Looks up the active DiagramBuilder via `getActiveBuilder()` and calls the
 * corresponding shape method with the converted props.
 */
export function jsx(
  type:
    | string
    | typeof Fragment
    | ((props: Record<string, unknown>) => Shape),
  props: Record<string, unknown>,
  _key?: string,
): Shape {
  if (type === Fragment) {
    throw new Error(
      "JSX fragments (<>...</>) are not supported in Bloom. " +
        "Use the `shapes` prop on a <g> element to group shapes.",
    );
  }

  if (typeof type === "function") {
    return type(props);
  }

  const builder = getActiveBuilder();
  if (builder === null) {
    throw new Error(
      "JSX shapes can only be created inside a DiagramBuilder context. " +
        "Use JSX within a forall() callback or directly after constructing a DiagramBuilder.",
    );
  }

  const methodName = elementToMethod[type];
  if (methodName === undefined) {
    throw new Error(
      `Unknown JSX element: "${type}". ` +
        `Supported elements: ${Object.keys(elementToMethod).join(", ")}`,
    );
  }

  const camelProps = kebabToCamel(props);
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return (builder as any)[methodName](camelProps) as Shape;
}

/** Alias for jsx — used when there are multiple children (same semantics for Bloom) */
export const jsxs = jsx;

/** Development-mode alias (used by bundlers in dev mode) */
export const jsxDEV = jsx;

// --- Shared prop interfaces (used in JSX.IntrinsicElements below) ---

interface CommonJSXProps {
  name?: string;
  "ensure-on-canvas"?: boolean;
  "interactive-only"?: boolean;
}

interface StrokeJSXProps {
  "stroke-width"?: Num;
  "stroke-style"?: string;
  "stroke-color"?: Color;
  "stroke-dasharray"?: string;
}

interface FillJSXProps {
  "fill-color"?: Color;
}

interface CenterJSXProps {
  center?: Vec2;
  drag?: boolean;
  "drag-constraint"?: DragConstraint;
}

interface RectSizeJSXProps {
  width?: Num;
  height?: Num;
}

interface RotateJSXProps {
  rotation?: Num;
}

interface ArrowJSXProps {
  "start-arrowhead-size"?: Num;
  "end-arrowhead-size"?: Num;
  "start-arrowhead"?: string;
  "end-arrowhead"?: string;
  "flip-start-arrowhead"?: boolean;
}

interface PolyJSXProps {
  points?: Vec2[];
  drag?: boolean;
  "drag-constraint"?: DragConstraint;
}

interface StringJSXProps {
  string?: string;
  "font-size"?: string;
}

// --- JSX namespace (TypeScript uses this for type-checking JSX expressions) ---

export namespace JSX {
  export type Element = Shape;

  export interface IntrinsicElements {
    /** SVG circle / Bloom Circle */
    circle: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps & { r?: Num };

    /** SVG ellipse / Bloom Ellipse */
    ellipse: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps & { rx?: Num; ry?: Num };

    /** SVG rect / Bloom Rectangle */
    rect: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps &
      RotateJSXProps &
      RectSizeJSXProps & { "corner-radius"?: Num };

    /** SVG line / Bloom Line */
    line: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      ArrowJSXProps & {
        start?: Vec2;
        end?: Vec2;
        "stroke-linecap"?: string;
        drag?: boolean;
        "drag-constraint"?: DragConstraint;
      };

    /** SVG path / Bloom Path */
    path: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      ArrowJSXProps & {
        d?: PathData;
        "stroke-linecap"?: string;
      };

    /** SVG polygon / Bloom Polygon */
    polygon: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      PolyJSXProps & { scale?: Num };

    /** SVG polyline / Bloom Polyline */
    polyline: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      PolyJSXProps & { scale?: Num; "stroke-linecap"?: string };

    /** SVG text / Bloom Text */
    text: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps &
      RectSizeJSXProps &
      RotateJSXProps &
      StringJSXProps & {
        visibility?: string;
        "font-family"?: string;
        "font-size-adjust"?: string;
        "font-stretch"?: string;
        "font-style"?: string;
        "font-variant"?: string;
        "font-weight"?: string;
        "text-anchor"?: string;
        "line-height"?: string;
        "alignment-baseline"?: string;
        "dominant-baseline"?: string;
        ascent?: Num;
        descent?: Num;
      };

    /** SVG image / Bloom Image */
    image: CommonJSXProps &
      CenterJSXProps &
      RectSizeJSXProps &
      RotateJSXProps & {
        svg?: string;
        "preserve-aspect-ratio"?: string;
      };

    /** SVG g (group) / Bloom Group */
    g: CommonJSXProps & {
      shapes?: Shape[];
      "clip-path"?: Exclude<Shape, Group>;
    };

    /** Penrose-specific Equation shape */
    equation: CommonJSXProps &
      FillJSXProps &
      CenterJSXProps &
      RectSizeJSXProps &
      RotateJSXProps &
      StringJSXProps & {
        ascent?: Num;
        descent?: Num;
      };
  }
}
