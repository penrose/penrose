import {
  Expr,
  Index,
  LitVec,
  Member,
  PenroseState,
  Rec,
  Var,
  Vec,
  compile, Shape, Path
} from "@penrose/core";
import { entries, Trio } from "@penrose/examples";
import seedrandom from "seedrandom";
import { PathCmd, PathDataV, PtListV, Value } from "@penrose/core/dist/types/value";

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

export const normalInPlace = (
  arr: Float64Array,
  rng: seedrandom.prng,
): void => {
  const d = arr.length;
  for (let i = 0; i < d; i += 2) {
    const u1 = rng.quick();
    const u2 = rng.quick();
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
): Map<string, Set<number>> => {
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

  const inputSets = new Map<string, Set<number>>();

  // Process each optimization stage
  for (const stage of state.optStages) {

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


    // AND with existing input mask for this stage
    const existingMask =
      state.constraintSets.get(stage)?.inputMask ||
      new Array(state.inputs.length).fill(true);

    const finalSet = new Set<number>();
    for (const idx of inputIndices) {
      if (existingMask[idx]) {
        finalSet.add(idx);
      }
    }

    inputSets.set(stage, finalSet);
  }

  return inputSets;
};

export const allFinite = (arr: Float64Array): boolean => {
  for (const value of arr) {
    if (!isFinite(value)) return false;
  }
  return true;
};

const areFieldsEqual = (
  a: Value<number>,
  b: Value<number>,
): boolean
 => {
  if (a.tag !== b.tag) return false;

  switch (a.tag) {
    case "FloatV":
    case "BoolV":
    case "StrV":
      return a.contents === b.contents;

    case "VectorV":
    case "TupV":
    case "ListV":
      return a.contents.reduce((acc, x, i) => acc && x === b.contents[i], true);

    default:
      throw new Error(`Unsupported value type for comparison: ${a.tag}`);
  }
}

const getComparableFields = <T>(
  shape: Shape<T>
): string[] => {
  const centerFields = ["center"];
  const rectFields = ["width", "height"];

  switch (shape.shapeType) {
    case "Circle":
      return ["r", ...centerFields];

    case "Ellipse":
      return ["rx", "ry", ...centerFields];

    case "Line":
      return ["start", "end"];

    case "Equation":
      return ["center"];

    case "Image":
      return [...rectFields, ...centerFields];

    case "Polygon":
      return ["points"];

    case "Polyline":
      return ["points"];

    case "Rectangle":
      return [...rectFields, ...centerFields];

    case "Text":
      return [...centerFields];

    default:
      return [];
  }
}

const getPathVertices = (
  cmds: PathCmd<number>[]
): [number, number][] => {
  const vertices: [number, number][] = [];
  let currentX = 0;
  let currentY = 0;
  let subpathStartX = 0;
  let subpathStartY = 0;

  function addVertex(x, y) {
    vertices.push([x, y]);
  }

  let startingNewSubpath = true;

  for (const { cmd: command, contents: paramsList } of cmds) {
    const params = paramsList.flatMap(x => x.contents);

    const isRelative = command === command.toLowerCase();
    const cmd = command.toUpperCase();

    switch (cmd) {
      case 'M': // Move to
      case 'L': // Line to
        for (let i = 0; i < params.length; i += 2) {
          const x = isRelative ? currentX + params[i] : params[i];
          const y = isRelative ? currentY + params[i + 1] : params[i + 1];

          currentX = x;
          currentY = y;

          if (cmd === 'M' && startingNewSubpath) {
            // First M command starts a new subpath
            subpathStartX = x;
            subpathStartY = y;
            startingNewSubpath = false;
          }

          addVertex(x, y);
        }
        break;

      case 'H': // Horizontal line to
        for (let i = 0; i < params.length; i++) {
          const x = isRelative ? currentX + params[i] : params[i];

          currentX = x;
          addVertex(x, currentY);
        }
        break;

      case 'V': // Vertical line to
        for (let i = 0; i < params.length; i++) {
          const y = isRelative ? currentY + params[i] : params[i];

          currentY = y;
          addVertex(currentX, y);
        }
        break;

      case 'C': // Cubic Bézier curve
        for (let i = 0; i < params.length; i += 6) {
          // Control points: (x1, y1), (x2, y2), end point: (x, y)
          const x = isRelative ? currentX + params[i + 4] : params[i + 4];
          const y = isRelative ? currentY + params[i + 5] : params[i + 5];

          currentX = x;
          currentY = y;
          addVertex(x, y);
        }
        break;

      case 'S': // Smooth cubic Bézier curve
      case 'Q': // Quadratic Bézier curve
        for (let i = 0; i < params.length; i += 4) {
          // Control point: (x2, y2), end point: (x, y)
          const x = isRelative ? currentX + params[i + 2] : params[i + 2];
          const y = isRelative ? currentY + params[i + 3] : params[i + 3];

          currentX = x;
          currentY = y;
          addVertex(x, y);
        }
        break;

      case 'T': // Smooth quadratic Bézier curve
        for (let i = 0; i < params.length; i += 2) {
          const x = isRelative ? currentX + params[i] : params[i];
          const y = isRelative ? currentY + params[i + 1] : params[i + 1];

          currentX = x;
          currentY = y;
          addVertex(x, y);
        }
        break;

      case 'A': // Elliptical arc
        for (let i = 0; i < params.length; i += 7) {
          // rx, ry, x-axis-rotation, large-arc-flag, sweep-flag, x, y
          const x = isRelative ? currentX + params[i + 5] : params[i + 5];
          const y = isRelative ? currentY + params[i + 6] : params[i + 6];

          currentX = x;
          currentY = y;
          addVertex(x, y);
        }
        break;

      case 'Z': // Close path
        // Return to the start of the current subpath, but don't add a vertex
        // since it's just connecting back to an existing vertex
        currentX = subpathStartX;
        currentY = subpathStartY;
        startingNewSubpath = true;
        break;
    }
  }

  return vertices;
}



export const findDiagramDistance2 = (
  shapes1: Shape<number>[],
  shapes2: Shape<number>[],
): Record<string, number> => {
  // ensure that shapes1 and shapes2 have same length and same types
  if (shapes1.length !== shapes2.length) {
    throw new Error("Shapes arrays must have the same length");
  }

  const n = shapes1.length;

  const totalDists: Record<string, number> = {};

  for (let i = 0; i < n; i++) {
    const shape1 = shapes1[i];
    const shape2 = shapes2[i];

    if (shape1.shapeType !== shape2.shapeType) {
      throw new Error("Shapes arrays must have the same types");
    }

    const fields = getComparableFields(shape1);

    const dists: Record<string, number> = {};

    for (const field of fields) {
      const value1: Value<number> = shape1[field];
      const value2: Value<number> = shape2[field];

      if (value1.tag !== value2.tag) {
        throw new Error(`Values for field ${field} must have the same type`);
      }

      // values should be FloatV<number> or as list type
      switch (value1.tag) {
        case "FloatV":
          dists[field] =
            (value1.contents - value2.contents) ** 2;
          break;

        case "VectorV":
        case "TupV":
        case "ListV":
          // assume same length
          dists[field] = value1
            .contents
            .reduce((acc, x, i) => acc + (x - value2.contents[i]) ** 2, 0);
          break;

        case "PtListV":
          dists[field] = value1
            .contents
            .reduce(
              (acc, [x1, y1], i) => {
                const [x2, y2] = (value2 as PtListV<number>).contents[i];
                return acc + (x1 - x2) ** 2 + (y1 - y2) ** 2;
              },
              0);
          break;

        default:
          throw new Error(`Unsupported value type for comparison field ${field}: ${value1.tag}`);
      }
    }

    // if value is path, compare vertices of path like ptlistv, but store
    // info as a field called "pathVertices"

    if (shape1.shapeType === "Path" && shape2.shapeType === "Path") {
      const path1 = shape1 as Path<number>;
      const path2 = shape2 as Path<number>;

      const vertices1 = getPathVertices(path1.d.contents);
      const vertices2 = getPathVertices(path2.d.contents);

      if (vertices1.length !== vertices2.length) {
        throw new Error("Paths must have the same number of vertices");
      }

      dists["pathVertices"] = vertices1.reduce(
        (acc, [x1, y1], i) => {
          const [x2, y2] = vertices2[i];
          return acc + (x1 - x2) ** 2 + (y1 - y2) ** 2;
        },
        0
      );
    }

    // accumulate distances for each field
    for (const [field, dist] of Object.entries(dists)) {
      if (!totalDists[field]) {
        totalDists[field] = 0;
      }
      totalDists[field] += dist;
    }
  }

  return totalDists;
}

export const getExampleNamesAndTrios = async  () => (
  await Promise.all(
    entries.map(async ([name, meta]) => {
      if (!meta.trio) return null;
      const trio = await meta.get();
      return [name, trio] as [string, Trio];
    }),
  )
).filter((x) => x !== null) as [string, Trio][];

export const diagramStdDev = (
  diagrams: Shape<number>[][],
): Record<string, number> => {
  const n = diagrams.length;

  if (n === 0) {
    return {};
  }

  if (n === 1) {
    // return 0 on all relevant fields
    return findDiagramDistance2(diagrams[0], diagrams[0]);
  }

  // i = idx / n, j = idx % n
  // i <= j
  let pairwiseDistMeans: Record<string, number> = {};
  let k = 0;
  for (let i = 0; i < n; i++) {
    for (let j = i; j < n; j++) {
      const dists = findDiagramDistance2(diagrams[i], diagrams[j]);

      for (const [field, dist] of Object.entries(dists)) {
        if (!(field in pairwiseDistMeans)) {
          pairwiseDistMeans[field] = 0;
        } else {
          pairwiseDistMeans[field] += (dist - pairwiseDistMeans[field]) / (k + 1);
        }
      }

      k++;
    }
  }

  const stdDevs: Record<string, number> = {};
  for (const [field, mean] of Object.entries(pairwiseDistMeans)) {
    stdDevs[field] = Math.sqrt(mean / 2);
  }

  return stdDevs;
}

export const getCompGraphEdges = (output: Expr): number => {
  let edgeCount = 0;
  const visited = new Set<Expr>();
  const stack = [output];

  while (stack.length > 0) {
    const node = stack.pop();
    if (!node || visited.has(node)) continue;

    visited.add(node);

    if (typeof node === "number") continue; // Skip numeric literals

    switch (node.tag) {
      case "Var":
        // Variable nodes have no outgoing edges
        break;

      case "Unary":
        edgeCount++; // Edge to param
        stack.push(node.param);
        break;

      case "Binary":
        edgeCount += 2; // Edges to left and right
        stack.push(node.left, node.right);
        break;

      case "Ternary":
        edgeCount += 3; // Edges to cond, then, and els
        stack.push(node.cond, node.then, node.els);
        break;

      case "Nary":
        edgeCount += node.params.length; // Edge to each param
        stack.push(...node.params);
        break;

      case "LitVec":
        edgeCount += node.elems.length; // Edge to each element
        stack.push(...node.elems);
        break;

      case "LitRec":
        const memberValues = Object.values(node.mems);
        edgeCount += memberValues.length; // Edge to each member
        memberValues.forEach((mem) => stack.push(mem));
        break;

      case "Index":
      case "Member": {
        const unfolded = unfold(node);
        if (typeof unfolded === "object" && unfolded.tag === node.tag) {
          // If unfolding didn't change the type, count edge to inner node
          edgeCount++;
          stack.push(
            (unfolded as any)[unfolded.tag === "Index" ? "vec" : "rec"],
          );
        } else {
          // If unfolding changed the type, count edge and continue with unfolded
          edgeCount++;
          stack.push(unfolded);
        }
        break;
      }

      case "Call":
        edgeCount += node.args.length; // Edge to each argument
        stack.push(...node.args);
        break;

      case "PolyRoots":
        edgeCount += node.coeffs.length; // Edge to each coefficient
        stack.push(...node.coeffs);
        break;

      case "Logic":
      case "Comp":
        edgeCount += 2; // Edges to left and right
        stack.push(node.left, node.right);
        break;

      case "Not":
        edgeCount++; // Edge to param
        stack.push(node.param);
        break;
    }
  }

  return edgeCount;
};

export const diagramCompGraphEdges = (diagram: PenroseState): number => {
  // find all outputs in objFns and constrFns, and sum their edges
  const outputs = [
    ...diagram.objFns.map((fn) => fn.output),
    ...diagram.constrFns.map((fn) => fn.output),
  ];

  return outputs.reduce((acc, output) => acc + getCompGraphEdges(output), 0);
}

