import { VarAD } from "./ad";
import { Value } from "./value";

export type Shape = IShape<number>;
export type ShapeAD = IShape<VarAD>;

export type Properties<T> = { [k: string]: Value<T> };

/**
 * A shape (Graphical Primitive Instance, aka GPI) in penrose has a type (_e.g._ `Circle`) and a set of properties (_e.g._ `center`). This type is specifically used for rendering. See {@link GPI} for the version used for the runtime.
 */
interface IShape<T> {
  shapeType: string;
  properties: Properties<T>;
}
