export const toScreen = (
  [x, y]: [number, number],
  canvasSize: [number, number]
) => {
  const [width, height] = canvasSize;
  return [width / 2 + x, height / 2 - y];
};
export const toHex = (rgba: [number, number, number, number]) => {
  return rgba.slice(0, 3).reduce((prev, cur) => {
    const hex = Math.round(255 * cur).toString(16);
    const padded = hex.length === 1 ? "0" + hex : hex;
    return prev + padded;
  }, "#");
};
export const containsEmptyLabels = (shapes: any[]) =>
  !shapes.every(([type, obj]: [string, any]) => {
    if (type === "Text") {
      return obj.w !== 0 && obj.h !== 0;
    }
    return true;
  });
