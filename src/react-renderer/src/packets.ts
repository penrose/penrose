import memoize from "fast-memoize";

const memoized = (p: any) => memoize((...args: any) => JSON.stringify(p(...args)));

export const autoStepToggle = memoized(() => ({ tag: "Cmd", contents: { command: "autostep" } }));

export const step = memoized(() => ({ tag: "Cmd", contents: { command: "step" } }));

export const resample = memoized(() => ( { tag: "Cmd", contents: { command: "resample" } }));



