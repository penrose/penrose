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
 *
 * Unknown SVG elements (defs, linearGradient, etc.) are registered as raw SVG
 * defs and injected into the rendered SVG output.
 */
import { getActiveBuilder } from "./core/builder.js";
import type { Num } from "@penrose/core";
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
  "children",
]);

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
): { bloomProps: Record<string, unknown>; rawAttrs: Record<string, string> } => {
  const shapeType = elementToShapeType[tag];
  const fieldTypes = shapeType
    ? penroseShapeFieldTypes.get(shapeType)
    : undefined;

  const bloomProps: Record<string, unknown> = {};
  const rawAttrs: Record<string, string> = {};

  for (const [origKey, val] of Object.entries(originalProps)) {
    const camelKey = origKey.replace(/-([a-z])/g, (_, c: string) =>
      c.toUpperCase(),
    );
    if (BLOOM_SPECIAL_PROPS.has(camelKey)) {
      // Special Bloom props — always go to builder
      continue; // "children" is handled separately
    } else if (fieldTypes && camelKey in fieldTypes) {
      // Known Penrose field — goes to builder (camelCase key)
      bloomProps[camelKey] = val;
    } else if (typeof val === "string" && fieldTypes !== undefined) {
      // String value for an unknown field on a known shape → raw SVG attr.
      // Keep the original key so setAttribute receives the correct SVG
      // attribute name (e.g. "paint-order", not "paintOrder").
      rawAttrs[origKey] = val;
    } else {
      // Num, array, or other — goes to builder (unknown fields are ignored by toPenroseShape)
      bloomProps[camelKey] = val;
    }
  }

  return { bloomProps, rawAttrs };
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
      const rawChildren = val;
      const childArr = Array.isArray(rawChildren)
        ? rawChildren
        : [rawChildren];
      for (const child of childArr) {
        if (isRawSvgElement(child)) {
          children.push(child);
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

/** Sentinel value for JSX fragments (not supported — throws at runtime) */
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
    | ((props: Record<string, unknown>) => Shape | RawSvgElement),
  props: Record<string, unknown>,
  _key?: string,
): Shape | RawSvgElement {
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
  if (methodName !== undefined) {
    // Known shape type — call builder method, separating raw SVG attrs
    const { bloomProps, rawAttrs } = separateProps(type, props);
    const finalProps =
      Object.keys(rawAttrs).length > 0
        ? { ...bloomProps, rawAttrs }
        : bloomProps;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (builder as any)[methodName](finalProps) as Shape;
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
  export type Element = Shape | RawSvgElement;

  export interface IntrinsicElements {
    /** SVG circle / Bloom Circle */
    circle: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps & { r?: Num; fill?: string };

    /** SVG ellipse / Bloom Ellipse */
    ellipse: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps & { rx?: Num; ry?: Num; fill?: string };

    /** SVG rect / Bloom Rectangle */
    rect: CommonJSXProps &
      StrokeJSXProps &
      FillJSXProps &
      CenterJSXProps &
      RotateJSXProps &
      RectSizeJSXProps & { "corner-radius"?: Num; fill?: string };

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
      shapes?: Array<Shape | RawSvgElement>;
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
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    [tag: string]: any;
  }
}
