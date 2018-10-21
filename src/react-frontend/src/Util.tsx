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

interface IPre {
  tag: string;
  contents: any;
}

export const clean = (tree: IPre) => {
  return tree.contents.shapes.map((shape: IPre) => {
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
