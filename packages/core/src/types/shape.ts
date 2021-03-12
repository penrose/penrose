import { Value } from "./value";

export type Shape = IShape;

export type Properties = { [k: string]: Value<number> };

/**
 * A shape (Graphical Primitive Instance, aka GPI) in penrose has a type (_e.g._ `Circle`) and a set of properties (_e.g._ `center`). This type is specifically used for rendering. See {@link GPI} for the version used for the runtime.
 */
interface IShape {
  shapeType: string;
  properties: Properties;
}
