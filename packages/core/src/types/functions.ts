import { Context } from "../shapes/Samplers";
import * as ad from "../types/ad";
import { ValueShapeT } from "./types";
import { Value } from "./value";

export type CompFuncBody = (context: Context, ...args: any[]) => Value<ad.Num>;
export type ObjFuncBody = (...args: any[]) => ad.Num;
export type ConstrFuncBody = (...args: any[]) => ad.Num;

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
