/**
 * Registry of paired parity checks for Bloom JSX examples.
 *
 * Each entry is one of:
 *  - { kind: "snapshot", name, jsx } — renders the JSX example and asserts
 *    the output matches a stored snapshot (regression guard for the JSX runtime).
 *  - { kind: "trio", name, trio, jsx, variation } — renders both a trio and a
 *    JSX port with the same seed and asserts they produce identical normalised SVGs.
 *    Use this only when the JSX port was written to exactly mirror the trio's
 *    shape-creation order and constraints (see plan for constraints).
 *
 * Currently all entries use snapshot mode because Bloom's DiagramBuilder always
 * creates an internal `_time` input before any shapes, which shifts the sampling
 * sequence relative to the trio compiler. Until that offset is accounted for,
 * exact trio↔JSX numerical parity cannot be guaranteed.
 */

export interface SnapshotEntry {
  kind: "snapshot";
  name: string;
  jsx: () => Promise<string>;
}

export interface TrioEntry {
  kind: "trio";
  name: string;
  trio: () => Promise<{ substance: string; style: string; domain: string; variation: string }>;
  jsx: () => Promise<string>;
  variation: string;
}

export type PairedEntry = SnapshotEntry | TrioEntry;

export const pairedExamples: PairedEntry[] = [
  {
    kind: "snapshot",
    name: "bloom/simple-circles",
    jsx: async () => (await import("./bloom/simple-circles.js")).default(),
  },
  {
    kind: "snapshot",
    name: "bloom/custom-components",
    jsx: async () => (await import("./bloom/custom-components.js")).default(),
  },
];
