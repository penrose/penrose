import memoize from "fast-memoize";
import { times } from "lodash";
import seedrandom from "seedrandom";
import { Properties } from "types/shapeTypes";

seedrandom("secret-seed", { global: true }); // HACK: constant seed for pseudorandomness

/**
 * Generate a random float. The maximum is exclusive and the minimum is inclusive
 * @param min minimum (inclusive)
 * @param max maximum (exclusive)
 */
export const randFloats = (
  count: number,
  [min, max]: [number, number]
): number[] => times(count, () => randFloat(min, max));

/**
 * Generate a random float. The maximum is exclusive and the minimum is inclusive
 * @param min minimum (inclusive)
 * @param max maximum (exclusive)
 */
export const randFloat = (min: number, max: number): number => {
  // TODO: better error reporting
  console.assert(
    max > min,
    "min should be smaller than max for random number generation!"
  );
  return Math.random() * (max - min) + min;
};

/**
 * Generate a random integer. The maximum is exclusive and the minimum is inclusive
 * @param min minimum (inclusive)
 * @param max maximum (exclusive)
 */
export const randInt = (min: number, max: number) => {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min)) + min;
};

/**
 * Find the Euclidean distance between two points
 * @param param0 point 1 [x1, y1]
 * @param param1 point 2 [x2, y2]
 */
export const dist = ([x1, y1]: [number, number], [x2, y2]: [number, number]) =>
  Math.sqrt(Math.pow(x1 - x2, 2) + Math.pow(y1 - y2, 2));

export const arrowheads = {
  "arrowhead-1": {
    width: 8,
    height: 8,
    viewbox: "0 0 8 8",
    refX: "4",
    refY: "4", // HACK: to avoid paths from bleeding through the arrowhead
    path: "M0,0 A30,30,0,0,0,8,4 A30,30,0,0,0,0,8 L2.5,4 z",
  },
  "arrowhead-2": {
    width: 9.95,
    height: 8.12,
    viewbox: "0 0 9.95 8.12",
    refX: "2.36", // HACK: to avoid paths from bleeding through the arrowhead
    refY: "4.06",
    path: "M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z",
  },
};

// calculates bounding box dimensions of a shape - used in inspector views
export const bBoxDims = (properties: Properties, shapeType: string) => {
  let [w, h] = [0, 0];
  if (shapeType === "Circle") {
    [w, h] = [
      (properties.r.contents as number) * 2,
      (properties.r.contents as number) * 2,
    ];
  } else if (shapeType === "Square") {
    [w, h] = [
      properties.side.contents as number,
      properties.side.contents as number,
    ];
  } else if (shapeType === "Ellipse") {
    [w, h] = [
      (properties.rx.contents as number) * 2,
      (properties.ry.contents as number) * 2,
    ];
  } else if (shapeType === "Arrow" || shapeType === "Line") {
    const [[sx, sy], [ex, ey]] = [
      properties.start.contents as [number, number],
      properties.end.contents as [number, number],
    ];
    const padding = 50; // Because arrow may be horizontal or vertical, and we don't want the size to be zero in that case
    [w, h] = [
      Math.max(Math.abs(ex - sx), padding),
      Math.max(Math.abs(ey - sy), padding),
    ];
  } else if (shapeType === "Path") {
    [w, h] = [20, 20]; // todo find a better measure for this... check with max?
  } else {
    [w, h] = [properties.w.contents as number, properties.h.contents as number];
  }
  return [w, h];
};

// export const Shadow = (props: { id: string }) => {
//   return (
//     <filter id={props.id} x="0" y="0" width="200%" height="200%">
//       <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5" />
//       <feGaussianBlur result="blurOut" in="offOut" stdDeviation="4" />
//       <feBlend in="SourceGraphic" in2="blurOut" mode="normal" />
//       <feComponentTransfer>
//         <feFuncA type="linear" slope="0.5" />
//       </feComponentTransfer>
//       <feMerge>
//         <feMergeNode />
//         <feMergeNode in="SourceGraphic" />
//       </feMerge>
//     </filter>
//   );
// };

export const toScreen = (
  [x, y]: [number, number],
  canvasSize: [number, number]
) => {
  const [width, height] = canvasSize;
  return [width / 2 + x, height / 2 - y];
};

export const penroseToSVG = (canvasSize: [number, number]) => {
  const [width, height] = canvasSize;
  const flipYStr = "matrix(1 0 0 -1 0 0)";
  const translateStr = "translate(" + width / 2 + ", " + height / 2 + ")";
  // Flip Y direction, then translate shape to origin mid-canvas
  return [translateStr, flipYStr].join(" ");
};

export const penroseTransformStr = (tf: any) => {
  const transformList = [
    tf.xScale,
    tf.ySkew,
    tf.xSkew,
    tf.yScale,
    tf.dx,
    tf.dy,
  ];
  const penroseTransform = "matrix(" + transformList.join(" ") + ")";
  return penroseTransform;
};

export const svgTransformString = (tf: any, canvasSize: [number, number]) => {
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
  // `tf` is `shape.transformation.contents`, an HMatrix from the backend
  // It is the *full* transform, incl. default transform
  // Do Penrose transform, then SVG
  const transformStr = [penroseToSVG(canvasSize), penroseTransformStr(tf)].join(
    " "
  );
  return transformStr;
};

// BUG: ptList needs to be properly typed
export const toPointListString = memoize(
  // Why memoize?
  (ptList: any[], canvasSize: [number, number]) =>
    ptList
      .map((coords: [number, number]) => {
        const pt = coords;
        return pt[0].toString() + " " + pt[1].toString();
      })
      .join(" ")
);

export const getAlpha = (color: any) => color.contents[3];

export const toHexRGB = (color: [number, number, number]): string => {
  return color.reduce((prev: string, cur: number) => {
    const hex = Math.round(255 * cur).toString(16);
    const padded = hex.length === 1 ? "0" + hex : hex;
    return prev + padded;
  }, "#");
};

// TODO nest this
function hsv2rgb(
  r1: number,
  g1: number,
  b1: number,
  m: number
): [number, number, number] {
  return [r1 + m, g1 + m, b1 + m];
}

// Expects H as angle in degrees, S in [0,100], L in [0,100] and converts the latter two to fractions.
// Returns rgb in range [0, 1]
// From https://github.com/d3/d3-hsv/blob/master/src/hsv.js
export const hsvToRGB = (
  hsv: [number, number, number]
): [number, number, number] => {
  const [h0, s0, v0] = hsv;
  const h = isNaN(h0) ? 0 : (h0 % 360) + Number(h0 < 0) * 360;
  const s = isNaN(h0) || isNaN(s0) ? 0 : s0 / 100.0;
  const v = v0 / 100.0;
  const c = v * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = v - c;

  return h < 60
    ? hsv2rgb(c, x, 0, m)
    : h < 120
    ? hsv2rgb(x, c, 0, m)
    : h < 180
    ? hsv2rgb(0, c, x, m)
    : h < 240
    ? hsv2rgb(0, x, c, m)
    : h < 300
    ? hsv2rgb(x, 0, c, m)
    : hsv2rgb(c, 0, x, m);
};

export const toHex = (color: any): string => {
  if (color.tag === "RGBA") {
    const rgba = color.contents;
    return toHexRGB(rgba.slice(0, 3));
  } else if (color.tag === "HSVA") {
    const hsv = color.contents.slice(0, 3);
    const rgb = hsvToRGB(hsv);
    return toHexRGB(rgb);
  } else {
    console.error("color type", color.tag, "unimplemented");
    return "";
  }
};

export const getAngle = (x1: number, y1: number, x2: number, y2: number) => {
  const x = x1 - x2;
  const y = y1 - y2;
  if (!x && !y) {
    return 0;
  }
  return (180 + (Math.atan2(-y, -x) * 180) / Math.PI + 360) % 360;
};

export const getLen = (x1: number, y1: number, x2: number, y2: number) => {
  return Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
};

export const loadImageElement = memoize(
  async (url: string): Promise<any> =>
    new Promise((resolve, reject) => {
      const img = new Image();
      img.onload = () => resolve(img);
      img.onerror = reject;
      img.src = url;
    })
);

// Load images asynchronously so we can send the dimensions to the backend and use it in the frontend

export const loadImages = async (allShapes: any[]) => {
  return Promise.all(
    allShapes.map(async ({ shapeType, properties }: any) => {
      if (shapeType === "ImageTransform") {
        const path = properties.path.contents;
        const fullPath = process.env.PUBLIC_URL + path;
        const loadedImage = await loadImageElement(fullPath);
        const obj2 = { ...properties };

        obj2.initWidth.contents = loadedImage.naturalWidth;
        obj2.initHeight.contents = loadedImage.naturalHeight;

        return { shapeType, properties: obj2 };
      } else {
        return { shapeType, properties };
      }
    })
  );
};

export const round2 = (n: number) => roundTo(n, 2);

// https://stackoverflow.com/questions/15762768/javascript-math-round-to-two-decimal-places
// Ported so string conversion works in typescript...
export const roundTo = (n: number, digits: number) => {
  let negative = false;

  if (digits === undefined) {
    digits = 0;
  }

  if (n < 0) {
    negative = true;
    n = n * -1;
  }

  const multiplicator = Math.pow(10, digits);
  const nNum = parseFloat((n * multiplicator).toFixed(11));
  const n2 = parseFloat((Math.round(nNum) / multiplicator).toFixed(2));
  let n3 = n2;

  if (negative) {
    n3 = parseFloat((n2 * -1).toFixed(2));
  }

  return n3;
};

// https://stackoverflow.com/questions/31084619/map-a-javascript-es6-map
// Basically Haskell's mapByValue (?)
export function mapMap(map: Map<any, any>, fn: any) {
  return new Map(Array.from(map, ([key, value]) => [key, fn(value, key, map)]));
}

/**
 * Safe wrapper for any function that might return `undefined`.
 * @borrows https://stackoverflow.com/questions/54738221/typescript-array-find-possibly-undefind
 * @param argument Possible unsafe function call
 * @param message Error message
 */
export const safe = <T extends {}>(
  argument: T | undefined | null,
  message: string
): T => {
  if (argument === undefined || argument === null) {
    throw new TypeError(message);
  }
  return argument;
};
