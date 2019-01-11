import memoize from "fast-memoize";

const memoized = (p: any) =>
  memoize((...args: any) => JSON.stringify(p(...args)));

export const autoStepToggle = memoized(() => ({
  tag: "Cmd",
  contents: { command: "autostep" }
}));

export const step = memoized(() => ({
  tag: "Cmd",
  contents: { command: "step" }
}));

export const resample = memoized(() => ({
  tag: "Cmd",
  contents: { command: "resample" }
}));

export const update = memoized((updatedShapes: any[]) => ({
  tag: "Update",
  contents: {
    shapes: updatedShapes.map(([name, obj]: [string, any]) => {
      return [name, { ...obj, rendered: undefined }];
    })
  }
}));

export const drag = memoized((id: string, dy: number, dx: number) => ({
  tag: "Drag",
  contents: {
    name: id,
    xm: -dx,
    ym: -dy
  }
}));
