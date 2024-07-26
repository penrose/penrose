import { useMemo } from "react";
import { Diagram } from "../builder/diagram.ts";

export class Input {
  public readonly tag = "Input";
  public readonly name: string;
  public readonly init?: number;

  private diagrams = new Set<Diagram>();
  private effectMap = new Map<Diagram, (val: number) => void>();

  private static nextId = 0;

  constructor(name?: string, init?: number) {
    this.name = name ?? `_input_${Input.nextId++}`;
    this.init = init;
  }

  private replaceEffects = () => {
    for (const [diagram, effect] of this.effectMap) {
      diagram.removeInputEffect(this.name, effect);
    }
    this.effectMap = new Map();
    for (const diagram of this.diagrams) {
      const effect = (val: number) => {
        for (const otherDiagram of this.diagrams) {
          if (otherDiagram === diagram) continue;
          otherDiagram.setInput(this.name, val, false);
        }
      };
      diagram.addInputEffect(this.name, effect);
      this.effectMap.set(diagram, effect);
    }
  };

  register = (diagram: Diagram) => {
    this.diagrams.add(diagram);
    this.replaceEffects();
  };
}

export const useInput = (name?: string, init?: number) => {
  return useMemo(() => new Input(name, init), [name, init]);
};

export const useInputs = (n: number) => {
  return useMemo(() => {
    const inputs = [];
    for (let i = 0; i < n; i++) {
      inputs.push(new Input());
    }
    return inputs;
  }, [n]);
};
