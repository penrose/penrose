import {
  compileTrio,
  prepareState,
  RenderStatic,
  stepUntilConvergence,
} from "@penrose/core";
import { SynthesizedSubstance } from "./Content";

export interface GridboxProps {
  substance: SynthesizedSubstance;
  domain: string;
  style: string;
}

const generateImg = async (sub: string, dsl: string, sty: string) => {
  const res = compileTrio(dsl, sub, sty);
  if (res.isOk()) {
    const state = await prepareState(res.value);
    const opt = stepUntilConvergence(state);
    if (opt.isErr()) {
      throw Error("optimization failed");
    }
    const optimized = opt.value;

    return RenderStatic(optimized).outerHTML;
  } else {
    throw Error(res.error());
  }
};

export function Gridbox(props: GridboxProps) {
  const img = generateImg(props.substance.prog, props.domain, props.style);
  return <div className="m-2">{img}</div>;
}
