import type { PlaneCoordinates } from "./point-set-topology.js";
import type { TrigonometricLoopData } from "./topological-vocabulary.js";

function parameter(value: number) {
  if (!Number.isFinite(value) || value < 0 || value > 1)
    throw new Error("A loop parameter must lie in [0,1]");
}

/** Evaluate a finite Fourier loop; integer harmonics agree exactly at both endpoints. */
export function trigonometricLoopValue(
  data: TrigonometricLoopData,
  time: number,
): PlaneCoordinates {
  parameter(time);
  if (
    ![...data.constant, ...data.cosine.flat(), ...data.sine.flat()].every(
      Number.isFinite,
    )
  )
    throw new Error("Loop coefficients must be finite");
  const angle = time === 1 ? 0 : 2 * Math.PI * time;
  const value: [number, number] = [...data.constant];
  data.cosine.forEach((c, i) => {
    for (const coordinate of [0, 1])
      value[coordinate] += c[coordinate] * Math.cos((i + 1) * angle);
  });
  data.sine.forEach((c, i) => {
    for (const coordinate of [0, 1])
      value[coordinate] += c[coordinate] * Math.sin((i + 1) * angle);
  });
  return value;
}

/** Rescale each half of a based-loop concatenation, with the common basepoint at the seam. */
export function concatenatedLoopParameter(time: number, split = 0.5) {
  parameter(time);
  if (!Number.isFinite(split) || !(split > 0 && split < 1))
    throw new Error("A concatenation needs an interior split");
  return time <= split
    ? { loop: 0 as const, time: time / split }
    : { loop: 1 as const, time: (time - split) / (1 - split) };
}

/** The source associativity homotopy varies the first and third durations while the middle stays 1/4. */
export function associativeLoopParameter(time: number, homotopyTime: number) {
  parameter(time);
  parameter(homotopyTime);
  const first = (1 + homotopyTime) / 4,
    second = (2 + homotopyTime) / 4;
  return time <= first
    ? { loop: 0 as const, time: time / first }
    : time <= second
    ? { loop: 1 as const, time: 4 * time - 1 - homotopyTime }
    : { loop: 2 as const, time: 1 - (4 * (1 - time)) / (2 - homotopyTime) };
}

/** Collapse a constant first/last segment continuously while preserving the endpoint basepoint. */
export function unitLoopParameter(
  time: number,
  homotopyTime: number,
  side: "left" | "right",
) {
  parameter(time);
  parameter(homotopyTime);
  const duration = 1 - homotopyTime / 2;
  return side === "right"
    ? Math.min(time / duration, 1)
    : Math.max((time - homotopyTime / 2) / duration, 0);
}

/** Book Proposition4's retracing homotopy: traverse less of a and return along the same path. */
export function inverseCancellationParameter(
  time: number,
  homotopyTime: number,
) {
  parameter(time);
  parameter(homotopyTime);
  return 2 * Math.min(time, 1 - time) * (1 - homotopyTime);
}
