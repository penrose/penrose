import { Context } from "../shapes/Samplers.js";
import * as ad from "../types/ad.js";
import { FunctionInternalWarning } from "./errors.js";
import { ValueShapeT } from "./types.js";
import { Value } from "./value.js";

export type MayWarn<T> = {
  value: T;
  warnings: FunctionInternalWarning[];
};

export type CompFuncBody = (
  context: Context,
  ...args: any[]
) => MayWarn<Value<ad.Num>>;
export type ObjFuncBody = (...args: any[]) => MayWarn<ad.Num>;
export type ConstrFuncBody = (...args: any[]) => MayWarn<ad.Num>;

export interface FuncParam {
  name: string;
  type: ValueShapeT;
  default?: Value<ad.Num>["contents"];
  description?: string;
}

export interface CompFunc {
  name: string;
  params: FuncParam[];
  body: CompFuncBody;
  returns: ValueShapeT;
  description?: string;
}

export interface ObjFunc {
  name: string;
  params: FuncParam[];
  body: ObjFuncBody;
  description?: string;
}

export interface ConstrFunc {
  name: string;
  params: FuncParam[];
  body: ConstrFuncBody;
  description?: string;
}
