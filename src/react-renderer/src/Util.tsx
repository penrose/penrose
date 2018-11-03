export const toScreen = (
  [x, y]: [number, number],
  canvasSize: [number, number]
) => {
  const [width, height] = canvasSize;
  return [width / 2 + x, height / 2 - y];
};
const toHex = (rgba: [number, number, number, number]) => {
  return rgba.slice(0, 3).reduce((prev, cur) => {
    const hex = Math.round(255 * cur).toString(16);
    const padded = hex.length === 1 ? "0" + hex : hex;
    return prev + padded;
  }, "#");
};
const toRgb = (hex: string) => {
  const bigint = parseInt(hex, 16);
  /* tslint:disable:no-bitwise */
  const r = (bigint >> 16) & 255;
  const g = (bigint >> 8) & 255;
  const b = bigint & 255;
  return [r / 255, g / 255, b / 255];
};

interface IPre {
  tag: string;
  contents: any;
}

export const containsEmptyLabels = (shapes: any[]) =>
  !shapes.every(([type, obj]: [string, any]) => {
    if (type === "Text") {
      return obj.w !== 0 && obj.h !== 0;
    }
    return true;
  });

export const clean = (shapes: any[]) => {
  return shapes.map((shape: IPre) => {
    const shapeN = { ...shape[1] };
    for (const key of Object.keys(shapeN)) {
      const val: IPre = shapeN[key];
      switch (val.tag) {
        case "FloatV":
          shapeN[key] = val.contents;
          break;
        case "IntV":
          shapeN[key] = val.contents;
          break;
        case "StrV":
          shapeN[key] = val.contents;
          break;
        case "ColorV":
          shapeN[key] = [toHex(val.contents), val.contents[3]];
          break;
        default:
          console.warn(
            `Value tag ${
              val.tag
            } unaccounted for. Defaulting to ${key}.contents.`
          );
          shapeN[key] = val.contents;
          break;
      }
    }
    return [shape[0], shapeN];
  });
};

export const serializeShape = ([name, obj]: [string, object]) => {
  const newObj = { ...obj };
  for (const key of Object.keys(newObj)) {
    let val = {};
    const orig = obj[key];
    switch (typeof orig) {
      case "number":
        // TODO: add int reconciliation
        // if (orig === parseInt(orig.toString(), 10)) {
        //   val = { tag: "IntV", contents: orig };
        // } else {
        val = { tag: "FloatV", contents: orig };
        // }
        break;
      case "object":
        if (orig instanceof String) {
          val = { tag: "StrV", contents: orig };
        } else if (Array.isArray(orig)) {
          // sorry, this is horrid
          if (key === "color") {
            val = {
              tag: "ColorV",
              contents: [...toRgb(orig[0].slice(1)), orig[1]]
            };
          }
        } else {
          console.error(`Could not serialize ${key}`);
        }
        break;
      case "string":
        val = { tag: "StrV", contents: orig };
        break;
      default:
        console.error(`Could not serialize ${key}`);
    }
    newObj[key] = val;
  }
  return [name, newObj];
};
