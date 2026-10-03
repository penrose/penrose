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
 * SVG cx/cy and line endpoints use a top-left origin with y increasing down.
 * They are converted to the optimizer's centered, y-up coordinates. Bloom's
 * center/start/end/points arrays retain their existing Penrose coordinates.
 * Numeric lengths accept numbers, numeric strings, and px; relative CSS units
 * are rejected because their geometry cannot be represented by the optimizer.
 * Penrose's kebab-case props (fill-color, etc.) remain available as aliases.
 *
 * Unknown SVG elements (defs, linearGradient, etc.) are registered as raw SVG
 * defs and injected into the rendered SVG output.
 * Functional components must finish construction synchronously.
 */
import { add, div, hexToRgba, type Num } from "@penrose/core";
import type { DiagramBuilder } from "./core/builder.js";
import { getActiveBuilder } from "./core/builder.js";
import type {
  Color,
  DragConstraint,
  Group,
  PathData,
  RawSvgElement,
  Shape,
  Vec2,
} from "./core/types.js";
import { penroseShapeFieldTypes, ShapeType } from "./core/types.js";

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

/** Map from JSX intrinsic element names to ShapeType (for rawAttrs detection) */
const elementToShapeType: Record<string, ShapeType> = {
  circle: ShapeType.Circle,
  ellipse: ShapeType.Ellipse,
  rect: ShapeType.Rectangle,
  line: ShapeType.Line,
  path: ShapeType.Path,
  polygon: ShapeType.Polygon,
  polyline: ShapeType.Polyline,
  text: ShapeType.Text,
  image: ShapeType.Image,
  g: ShapeType.Group,
  equation: ShapeType.Equation,
};

/**
 * Props that are special Bloom props not in penroseShapeFieldTypes but
 * should still be passed to the builder method (not treated as raw SVG attrs).
 */
const BLOOM_SPECIAL_PROPS = new Set([
  "drag",
  "dragConstraint",
  "interactiveOnly",
]);

export interface JSXFragment {
  readonly _fragment: true;
  readonly children: JSXChild[];
}

/** Children can be nested through JavaScript maps and conditional expressions. */
export type JSXChild =
  | Shape
  | RawSvgElement
  | JSXFragment
  | string
  | number
  | boolean
  | null
  | undefined
  | JSXChild[];

const flattenChildren = (children: unknown): unknown[] => {
  if (children == null || typeof children === "boolean") return [];
  if (Array.isArray(children)) return children.flatMap(flattenChildren);
  if (typeof children === "object" && "_fragment" in children) {
    return flattenChildren((children as JSXFragment).children);
  }
  return [children];
};

const isShape = (value: unknown): value is Shape =>
  typeof value === "object" && value !== null && "shapeType" in value;

const length = (value: unknown, prop: string): Num => {
  let parsed = value;
  if (typeof value === "string") {
    const match = value
      .trim()
      .match(/^([+-]?(?:\d+\.?\d*|\.\d+)(?:e[+-]?\d+)?)(?:px)?$/i);
    if (!match) {
      throw new Error(
        `${prop} requires a numeric SVG length or px; relative CSS units are unsupported.`,
      );
    }
    parsed = Number(match[1]);
  }
  if (typeof parsed === "number") {
    if (!Number.isFinite(parsed)) throw new Error(`${prop} must be finite.`);
    return parsed;
  }
  if (typeof parsed === "object" && parsed !== null && "tag" in parsed) {
    return parsed as Num;
  }
  throw new Error(`${prop} requires a number or a Penrose numeric expression.`);
};

const nativeGeometry = new Set(["cx", "cy", "x", "y", "x1", "y1", "x2", "y2"]);
const unsupportedGeometry = new Set(["transform", "viewBox"]);
const geometryFields = new Set([
  "r",
  "rx",
  "ry",
  "width",
  "height",
  "d",
  "points",
]);

const namedColors: Record<string, string> = {
  black: "000000",
  silver: "c0c0c0",
  gray: "808080",
  grey: "808080",
  white: "ffffff",
  maroon: "800000",
  red: "ff0000",
  purple: "800080",
  fuchsia: "ff00ff",
  green: "008000",
  lime: "00ff00",
  olive: "808000",
  yellow: "ffff00",
  navy: "000080",
  blue: "0000ff",
  teal: "008080",
  aqua: "00ffff",
};

/** Resolve common SVG paints into optimizer colors; paint servers stay raw. */
const paintColor = (value: string): Color | undefined => {
  const paint = value.trim().toLowerCase();
  if (paint === "none" || paint === "transparent") return [0, 0, 0, 0];
  const hex =
    namedColors[paint] ?? (paint.startsWith("#") ? paint.slice(1) : undefined);
  if (hex !== undefined && /^[\da-f]+$/i.test(hex)) return hexToRgba(hex);
  const rgb = paint.match(
    /^rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)(?:\s*,\s*([\d.]+))?\s*\)$/,
  );
  if (rgb) {
    const channels = rgb.slice(1, 4).map(Number);
    const alpha = rgb[4] === undefined ? 1 : Number(rgb[4]);
    if (channels.every((v) => v <= 255) && alpha <= 1) {
      return [...channels.map((v) => v / 255), alpha];
    }
  }
  return undefined;
};

/**
 * Separate props into Bloom-specific props (passed to builder) and
 * raw SVG attributes (string values for unknown fields, applied post-render).
 *
 * Iterates the original (kebab-case) props so that rawAttrs keys are preserved
 * in their original form (e.g. `paint-order`, not `paintOrder`). bloomProps
 * keys are converted to camelCase for the builder.
 */
const separateProps = (
  tag: string,
  originalProps: Record<string, unknown>,
): {
  bloomProps: Record<string, unknown>;
  rawAttrs: Record<string, string>;
} => {
  const shapeType = elementToShapeType[tag];
  const fieldTypes = shapeType
    ? penroseShapeFieldTypes.get(shapeType)
    : undefined;

  const bloomProps: Record<string, unknown> = {};
  const rawAttrs: Record<string, string> = {};

  for (const [origKey, val] of Object.entries(originalProps)) {
    if (
      val === undefined ||
      origKey === "children" ||
      nativeGeometry.has(origKey)
    )
      continue;
    if (unsupportedGeometry.has(origKey)) {
      throw new Error(
        `${origKey} is unsupported on optimized shapes; express geometry with Bloom fields.`,
      );
    }
    const camelKey = origKey.replace(/-([a-z])/g, (_, c: string) =>
      c.toUpperCase(),
    );
    if (BLOOM_SPECIAL_PROPS.has(camelKey)) {
      bloomProps[camelKey] = val;
    } else if (
      (origKey === "fill" || origKey === "stroke") &&
      typeof val === "string"
    ) {
      const field = origKey === "fill" ? "fillColor" : "strokeColor";
      const color = paintColor(val);
      if (color && fieldTypes && field in fieldTypes) bloomProps[field] = color;
      // Keep SVG's exact paint spelling and support gradients / CSS variables.
      rawAttrs[origKey] = val;
    } else if (fieldTypes && camelKey in fieldTypes) {
      const fieldType = fieldTypes[camelKey];
      if (fieldType === "FloatV") {
        const numeric = length(val, origKey);
        if (
          typeof numeric === "number" &&
          numeric < 0 &&
          [
            "r",
            "rx",
            "ry",
            "width",
            "height",
            "strokeWidth",
            "cornerRadius",
          ].includes(camelKey)
        ) {
          throw new Error(`${origKey} cannot be negative.`);
        }
        bloomProps[camelKey] = numeric;
      } else if (camelKey === "fontSize") {
        const size = length(val, origKey);
        if (typeof size !== "number" || size <= 0) {
          throw new Error("font-size requires a positive fixed size in px.");
        }
        bloomProps.fontSize = `${size}px`;
      } else if (camelKey === "d" && typeof val === "string") {
        throw new Error(
          "Optimized paths require PathData commands; SVG path strings are unsupported.",
        );
      } else if (camelKey === "points" && typeof val === "string") {
        throw new Error(
          "Optimized polygon points require Penrose Vec2 arrays; SVG point strings are unsupported.",
        );
      } else {
        bloomProps[camelKey] = val;
      }
    } else if (geometryFields.has(origKey)) {
      throw new Error(`SVG ${origKey} is unsupported on optimized ${tag}.`);
    } else if (
      (typeof val === "string" || typeof val === "number") &&
      fieldTypes !== undefined
    ) {
      // String value for an unknown field on a known shape → raw SVG attr.
      // Keep the original key so setAttribute receives the correct SVG
      // attribute name (e.g. "paint-order", not "paintOrder").
      rawAttrs[origKey] = String(val);
    } else {
      throw new Error(
        `Unsupported ${tag} prop ${origKey}; it cannot be silently ignored.`,
      );
    }
  }

  return { bloomProps, rawAttrs };
};

/** Validate before creating a shape so a rejected element has no side effects. */
const svgCoordinates = (
  tag: string,
  props: Record<string, unknown>,
): Record<string, Num> => {
  const present = (key: string) => props[key] !== undefined;
  const accepted =
    tag === "circle" || tag === "ellipse"
      ? ["cx", "cy"]
      : tag === "line"
      ? ["x1", "y1", "x2", "y2"]
      : tag === "rect"
      ? ["x", "y"]
      : [];
  const coordinates: Record<string, Num> = {};
  for (const key of nativeGeometry) {
    if (!present(key)) continue;
    if (!accepted.includes(key)) {
      throw new Error(
        `SVG ${key} is unsupported on optimized ${tag}; use its Penrose geometry fields.`,
      );
    }
    coordinates[key] = length(props[key], key);
  }
  const pairs =
    tag === "line"
      ? [
          ["start", "x1", "y1"],
          ["end", "x2", "y2"],
        ]
      : [["center", ...accepted]];
  for (const [field, ...keys] of pairs) {
    if (keys.some(present) && present(field)) {
      throw new Error(
        `Use either SVG ${keys.join("/")} or Penrose ${field}, not both.`,
      );
    }
  }
  return coordinates;
};

const applySvgGeometry = (
  builder: DiagramBuilder,
  tag: string,
  coordinates: Record<string, Num>,
  shape: Shape,
): void => {
  const present = (key: string) => coordinates[key] !== undefined;
  const coordinate = (key: string): Num => coordinates[key] ?? 0;
  if (tag === "circle" || tag === "ellipse") {
    if (present("cx") || present("cy")) {
      (shape as Shape & { center: Vec2 }).center = builder.svgPoint([
        coordinate("cx"),
        coordinate("cy"),
      ]);
    }
  } else if (tag === "line") {
    for (const [field, x, y] of [
      ["start", "x1", "y1"],
      ["end", "x2", "y2"],
    ] as const) {
      if (present(x) || present(y)) {
        (shape as Shape & { start: Vec2; end: Vec2 })[field] = builder.svgPoint(
          [coordinate(x), coordinate(y)],
        );
      }
    }
  } else if (tag === "rect") {
    if (present("x") || present("y")) {
      const rect = shape as Shape & { center: Vec2; width: Num; height: Num };
      rect.center = builder.svgPoint([
        add(coordinate("x"), div(rect.width, 2)),
        add(coordinate("y"), div(rect.height, 2)),
      ]);
    }
  }
};

/** Create a RawSvgElement from an unknown JSX element */
const createRawSvgElement = (
  tag: string,
  props: Record<string, unknown>,
): RawSvgElement => {
  const attrs: Record<string, string> = {};
  const children: RawSvgElement[] = [];

  for (const [key, val] of Object.entries(props)) {
    if (key === "children") {
      for (const child of flattenChildren(val)) {
        if (isRawSvgElement(child)) {
          children.push(child);
        } else {
          throw new Error(
            `Raw SVG ${tag} children must be raw SVG elements; optimized shapes belong in a g.`,
          );
        }
      }
    } else if (typeof val === "string" || typeof val === "number") {
      // Convert kebab-case attr names back to SVG native (they came in as raw kebab-case props)
      attrs[key] = String(val);
    }
  }

  return { _rawSvg: true, tag, attrs, children };
};

/** Type guard for RawSvgElement */
export const isRawSvgElement = (val: unknown): val is RawSvgElement =>
  typeof val === "object" &&
  val !== null &&
  "_rawSvg" in val &&
  (val as RawSvgElement)._rawSvg === true;

/** Fragments preserve a sequence of children without creating a shape. */
export const Fragment: unique symbol = Symbol("Fragment");

/**
 * JSX factory function. Called by the TypeScript compiler for every JSX element.
 *
 * - For known Bloom shape elements (circle, rect, etc.): calls the corresponding
 *   builder method. String props for unknown fields become rawAttrs (applied post-render).
 * - For unknown SVG elements (defs, linearGradient, etc.): creates a RawSvgElement
 *   and registers it with the active builder as a raw SVG def.
 * - For functional components: calls the function directly.
 */
export function jsx(
  type:
    | string
    | typeof Fragment
    | ((props: Record<string, unknown>) => Shape | RawSvgElement | JSXFragment),
  props: Record<string, unknown>,
  _key?: string,
): Shape | RawSvgElement | JSXFragment {
  // React-style keys have no meaning in an eager diagram construction.
  void _key;
  if (type === Fragment) {
    return {
      _fragment: true,
      children: flattenChildren(props.children) as JSXChild[],
    };
  }

  if (typeof type === "function") {
    if (
      ["[object AsyncFunction]", "[object AsyncGeneratorFunction]"].includes(
        Object.prototype.toString.call(type),
      )
    ) {
      throw new Error("JSX functional components must be synchronous.");
    }
    const result = type(props);
    if (
      result !== null &&
      typeof result === "object" &&
      "then" in result &&
      typeof result.then === "function"
    ) {
      throw new Error("JSX functional components must be synchronous.");
    }
    return result;
  }

  const builder = getActiveBuilder();
  if (builder === null) {
    throw new Error(
      "JSX shapes can only be created inside a DiagramBuilder context. " +
        "Use JSX within a forall() callback or directly after constructing a DiagramBuilder.",
    );
  }

  const methodName = elementToMethod[type];
  if (methodName !== undefined) {
    // Known shape type — call builder method, separating raw SVG attrs
    const { bloomProps, rawAttrs } = separateProps(type, props);
    const coordinates = svgCoordinates(type, props);
    const children = flattenChildren(props.children);
    if (type === "text" || type === "equation") {
      if (children.length > 0) {
        if (props.string !== undefined)
          throw new Error(
            "Use either text children or the string prop, not both.",
          );
        if (
          children.some(
            (child) => typeof child !== "string" && typeof child !== "number",
          )
        ) {
          throw new Error(`${type} children must be strings or numbers.`);
        }
        bloomProps.string = children.join("");
      }
    } else if (type === "g") {
      const members = [...flattenChildren(props.shapes), ...children];
      if (members.some((child) => !isShape(child) && !isRawSvgElement(child))) {
        throw new Error(
          "Group children must be shapes or raw SVG definitions.",
        );
      }
      bloomProps.shapes = members.filter(isShape);
    } else if (children.length > 0) {
      throw new Error(`Optimized ${type} does not accept children.`);
    }
    const finalProps =
      Object.keys(rawAttrs).length > 0
        ? { ...bloomProps, rawAttrs }
        : bloomProps;
    const shape = (builder as any)[methodName](finalProps) as Shape;
    applySvgGeometry(builder, type, coordinates, shape);
    return shape;
  } else {
    // Unknown SVG element — raw def (defs, linearGradient, stop, filter, etc.)
    // Props for raw elements are passed as-is (kebab-case) since they go directly to SVG
    const rawEl = createRawSvgElement(type, props);
    builder.addRawSvgDef(rawEl);
    return rawEl;
  }
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
  "stroke-width"?: Num | string;
  "stroke-style"?: string;
  "stroke-color"?: Color;
  "stroke-dasharray"?: string;
  stroke?: string;
}

interface FillJSXProps {
  "fill-color"?: Color;
  fill?: string;
}

interface CenterJSXProps {
  center?: Vec2;
  drag?: boolean;
  "drag-constraint"?: DragConstraint;
}

interface RectSizeJSXProps {
  width?: Num | string;
  height?: Num | string;
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
  "font-size"?: number | string;
  children?: JSXChild;
}

// --- JSX namespace (TypeScript uses this for type-checking JSX expressions) ---

// The TypeScript JSX protocol requires this exported namespace.
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace JSX {
  export type Element = Shape | RawSvgElement | JSXFragment;
  export interface ElementChildrenAttribute {
    children: unknown;
  }

  export interface IntrinsicElements {
    /** SVG circle / Bloom Circle */
    circle: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps & {
        r?: Num | string;
        cx?: Num | string;
        cy?: Num | string;
      };

    /** SVG ellipse / Bloom Ellipse */
    ellipse: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps & {
        rx?: Num | string;
        ry?: Num | string;
        cx?: Num | string;
        cy?: Num | string;
      };

    /** SVG rect / Bloom Rectangle */
    rect: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps &
      RotateJSXProps &
      RectSizeJSXProps & {
        "corner-radius"?: Num | string;
        x?: Num | string;
        y?: Num | string;
      };

    /** SVG line / Bloom Line */
    line: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      ArrowJSXProps & {
        start?: Vec2;
        end?: Vec2;
        x1?: Num | string;
        y1?: Num | string;
        x2?: Num | string;
        y2?: Num | string;
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
      shapes?: JSXChild[];
      children?: JSXChild;
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

    /** Any other SVG element — treated as a raw SVG def (defs, linearGradient, etc.) */
    [tag: string]: any;
  }
}
