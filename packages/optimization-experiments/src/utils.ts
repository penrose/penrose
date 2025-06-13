import {
  Expr,
  Index,
  LitVec,
  Member,
  PenroseState,
  Rec,
  Var,
  Vec,
  compile,
} from "@penrose/core";
import { Trio } from "@penrose/examples";

export const removeStaging = (state: PenroseState): PenroseState => {
  // "or" together all masks, and replace optStages with only one ("default")
  const combinedMasks = Array.from(state.constraintSets.values()).reduce(
    (acc, mask) => ({
      inputMask: acc.inputMask.map((val, idx) => val || mask.inputMask[idx]),
      objMask: acc.objMask.map((val, idx) => val || mask.objMask[idx]),
      constrMask: acc.constrMask.map((val, idx) => val || mask.constrMask[idx]),
    }),
    {
      inputMask: new Array(state.inputs.length).fill(false),
      objMask: new Array(state.objFns.length).fill(false),
      constrMask: new Array(state.constrFns.length).fill(false),
    },
  );

  state.constraintSets = new Map([["default", combinedMasks]]);
  state.optStages = ["default"];
  state.currentStageIndex = 0;

  return state;
};

export const compileTrio = async (trio: Trio): Promise<PenroseState | null> => {
  const style = trio.style.reduce((acc, s) => acc + s.contents, "");
  const result = await compile({
    substance: trio.substance,
    style,
    domain: trio.domain,
    variation: trio.variation,
    excludeWarnings: trio.excludeWarnings || [],
  });
  if (result.isErr()) {
    return null;
  }
  return result.value;
};

export const normal = (d: number): number[] => {
  const arr = new Array(d);
  for (let i = 0; i < d; i += 2) {
    const u1 = Math.random();
    const u2 = Math.random();
    const r = Math.sqrt(-2 * Math.log(u1));
    const theta = 2 * Math.PI * u2;
    arr[i] = r * Math.cos(theta);
    if (i + 1 < d) {
      arr[i + 1] = r * Math.sin(theta);
    }
  }
  return arr;
};

// Indexes into an index or member type as many times as is possible statically
const unfold = (node: Index | Member): Expr => {
  const innerProp = node.tag === "Index" ? "vec" : "rec";

  const inner: Vec | Rec = (node as any)[innerProp];

  const unfoldedInner = (
    inner.tag === "Index" || inner.tag === "Member" ? unfold(inner) : inner
  ) as Vec | Rec;

  switch (unfoldedInner.tag) {
    case "Index":
    case "Member":
    case "Call":
    case "PolyRoots":
      return {
        ...node,
        [innerProp]: unfoldedInner,
      };

    case "LitRec":
      return unfoldedInner.mems[(node as Member).member];

    case "LitVec":
      return unfoldedInner.elems[(node as Index).index];
  }
};

export const normalInPlace = (arr: Float64Array): void => {
  const d = arr.length;
  for (let i = 0; i < d; i += 2) {
    const u1 = Math.random();
    const u2 = Math.random();
    const r = Math.sqrt(-2 * Math.log(u1));
    const theta = 2 * Math.PI * u2;
    arr[i] = r * Math.cos(theta);
    if (i + 1 < d) {
      arr[i + 1] = r * Math.sin(theta);
    }
  }
};

/**
 * Calculates input masks for each optimization stage based on analyzing the
 * graph structure of compiled function outputs. The resulting masks are ANDed with
 * the existing input masks from state.constraintSets to filter only inputs that
 * are both enabled and can affect the energy.
 */
export const calculateDependentInputs = (
  state: PenroseState,
): Map<string, boolean[]> => {
  const inputMasks = new Map<string, boolean[]>();

  const varToIdxMap = new Map<Var, number>();
  // Create a map of variable names to their indices
  state.inputs.forEach((input, index) => {
    varToIdxMap.set(input.handle, index);
  });

  // Helper function to extract variable dependencies from a compiled function output graph
  const extractInputDependencies = (output: Expr): Set<number> => {
    const dependencies = new Set<number>();
    const visited = new Set<Expr>();
    const stack = [output];

    while (stack.length > 0) {
      const node = stack.pop();
      if (!node || visited.has(node)) continue;

      visited.add(node);

      if (typeof node === "number") continue; // Skip numeric literals

      switch (node.tag) {
        case "Var": {
          const index = varToIdxMap.get(node);
          if (index !== undefined) {
            dependencies.add(index);
          }
          break;
        }

        case "Unary":
          stack.push(node.param);
          break;

        case "Binary":
          stack.push(node.left, node.right);
          break;

        case "Ternary":
          stack.push(node.cond, node.then, node.els);
          break;

        case "Nary":
          stack.push(...node.params);
          break;

        case "LitVec":
          stack.push(...node.elems);
          break;

        case "LitRec":
          Object.values(node.mems).forEach((mem) => stack.push(mem));
          break;

        case "Index":
        case "Member": {
          const unfolded = unfold(node);
          if (typeof unfolded === "object" && unfolded.tag === node.tag) {
            // If unfolding didn't change the type, push the inner node
            stack.push(
              (unfolded as any)[unfolded.tag === "Index" ? "vec" : "rec"],
            );
          } else {
            stack.push(unfolded);
          }
          break;
        }

        case "Call":
          stack.push(...node.args);
          break;

        case "PolyRoots":
          stack.push(...node.coeffs);
          break;

        case "Logic":
        case "Comp":
          stack.push(node.left, node.right);
          break;

        case "Not":
          stack.push(node.param);
          break;
      }
    }

    return dependencies;
  };

  // Process each optimization stage
  for (const stage of state.optStages) {
    const dependencyMask = new Array(state.inputs.length).fill(false);

    // Get functions active in this stage
    const activeOutputs = [...state.objFns, ...state.constrFns]
      .filter((fn) => fn.optStages === "All" || fn.optStages.has(stage))
      .map((fn) => fn.output);

    const totalExpr: LitVec = {
      tag: "LitVec",
      elems: activeOutputs,
    };

    // Analyze dependencies for active functions
    const inputIndices = extractInputDependencies(totalExpr);

    // Mark dependent inputs as true
    inputIndices.forEach((index) => {
      if (index >= 0 && index < state.inputs.length) {
        dependencyMask[index] = true;
      }
    });

    // AND with existing input mask for this stage
    const existingMask =
      state.constraintSets.get(stage)?.inputMask ||
      new Array(state.inputs.length).fill(true);

    const finalMask = dependencyMask.map(
      (canAffect, index) => canAffect && existingMask[index],
    );

    inputMasks.set(stage, finalMask);
  }

  return inputMasks;
};

export const vdot = (a: Float64Array, b: Float64Array): number => {
  let result = 0;
  for (let i = 0; i < a.length; i++) {
    result += a[i] * b[i];
  }
  return result;
};
