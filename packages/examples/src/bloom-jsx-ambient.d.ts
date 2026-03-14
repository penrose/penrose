/**
 * Ambient declarations for @penrose/bloom/jsx-runtime.
 *
 * In the published package these types come from the "exports" field in
 * bloom/package.json. In the worktree development context, where node_modules
 * resolves @penrose/bloom to the pre-existing published package, we declare
 * the module here so TypeScript accepts JSX files that use:
 *
 *   /** @jsxImportSource @penrose/bloom *\/
 */
declare module "@penrose/bloom/jsx-runtime" {
  import type { Shape } from "@penrose/bloom";

  export function jsx(
    type: string | ((props: Record<string, unknown>) => Shape),
    props: Record<string, unknown>,
    key?: string,
  ): Shape;

  export function jsxs(
    type: string | ((props: Record<string, unknown>) => Shape),
    props: Record<string, unknown>,
    key?: string,
  ): Shape;

  export const Fragment: unique symbol;

  export namespace JSX {
    type Element = Shape;
    // Allow any intrinsic element; full prop types come from the published package.
    interface IntrinsicElements {
      [elemName: string]: Record<string, unknown>;
    }
  }
}

declare module "@penrose/bloom/jsx-dev-runtime" {
  export * from "@penrose/bloom/jsx-runtime";
  export { jsx as jsxDEV } from "@penrose/bloom/jsx-runtime";
}
