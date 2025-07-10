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

/**
 * Finds the optimal perfect matching between two sets using the Hungarian algorithm.
 * Computes minimum weight perfect matching in O(n³) time.
 * 
 * @param setA First set of elements
 * @param setB Second set of elements  
 * @param metric Distance/cost function between elements
 * @returns Array of pairs [a, b] representing the optimal matching
 */
export const findOptimalMatching = <T>(
  setA: Set<T>,
  setB: Set<T>,
  metric: (a: T, b: T) => number
): [T, T][] => {
  const arrayA = Array.from(setA);
  const arrayB = Array.from(setB);
  const n = Math.max(arrayA.length, arrayB.length);
  
  if (n === 0) return [];
  
  // Create cost matrix, padding with high costs for unequal set sizes
  const INF = 1e9;
  const cost = Array(n).fill(null).map(() => Array(n).fill(INF));
  
  for (let i = 0; i < arrayA.length; i++) {
    for (let j = 0; j < arrayB.length; j++) {
      cost[i][j] = metric(arrayA[i], arrayB[j]);
    }
  }
  
  // Hungarian algorithm implementation
  const u = Array(n + 1).fill(0); // potential for workers
  const v = Array(n + 1).fill(0); // potential for jobs
  const p = Array(n + 1).fill(0); // assignment
  const way = Array(n + 1).fill(0); // path reconstruction
  
  for (let i = 1; i <= n; i++) {
    p[0] = i;
    let j0 = 0;
    const minv = Array(n + 1).fill(INF);
    const used = Array(n + 1).fill(false);
    
    do {
      used[j0] = true;
      const i0 = p[j0];
      let delta = INF;
      let j1 = 0;
      
      for (let j = 1; j <= n; j++) {
        if (!used[j]) {
          const cur = cost[i0 - 1][j - 1] - u[i0] - v[j];
          if (cur < minv[j]) {
            minv[j] = cur;
            way[j] = j0;
          }
          if (minv[j] < delta) {
            delta = minv[j];
            j1 = j;
          }
        }
      }
      
      for (let j = 0; j <= n; j++) {
        if (used[j]) {
          u[p[j]] += delta;
          v[j] -= delta;
        } else {
          minv[j] -= delta;
        }
      }
      
      j0 = j1;
    } while (p[j0] !== 0);
    
    do {
      const j1 = way[j0];
      p[j0] = p[j1];
      j0 = j1;
    } while (j0);
  }
  
  // Extract matching pairs, filtering out padding
  const result: [T, T][] = [];
  for (let j = 1; j <= n; j++) {
    const i = p[j] - 1;
    const jIdx = j - 1;
    if (i < arrayA.length && jIdx < arrayB.length) {
      result.push([arrayA[i], arrayB[jIdx]]);
    }
  }
  
  return result;
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

const arePathCmdsComparable = (
  path1: PathDataV<number>,
  path2: PathDataV<number>
): boolean => {
  const cmds1 = path1.contents;
  const cmds2 = path2.contents;

  if (cmds1.length !== cmds2.length) return false;

  for (let i = 0; i < cmds1.length; i++) {
    if (cmds1[i].cmd !== cmds2[i].cmd) return false;
  }

  return true;
}

const arePointsComparable = (
  points1: PtListV<number>,
  points2: PtListV<number>
): boolean => {
  return points1.contents.length === points2.contents.length;
}

const areComparable = (
  shape1: Shape<number>,
  shape2: Shape<number>,
): boolean => {
  if (shape1.shapeType !== shape2.shapeType)
    return false;

  const distinguishingFields = getDistinguishingFields(shape1);
  for (const field of distinguishingFields) {
    if (!areFieldsEqual(
      shape1[field] as Value<number>,
      shape2[field] as Value<number>
    )) {
      return false;
    }
  }

  if (shape1.shapeType === "Path" && !arePathCmdsComparable(shape1.d, shape2.d))
    return false;

  if ((shape1.shapeType === "Polygon" || shape1.shapeType === "Polyline")
    && !arePointsComparable(shape1.points, shape2.points))
    return false;

  return true;
}

// To github copilot: ignore all the commented code above when autocompleting
const getComparableLists = (
  shapes: Shape<number>[]
): Shape<number>[][] => {
  const lists: Shape<number>[][] = [];

  for (const shape of shapes) {
    let foundExisting = false;
    for (const l of lists) {
      if (areComparable(shape, l[0])) {
        l.push(shape);
        foundExisting = true;
        break;
      }
    }

    if (!foundExisting) {
      lists.push([shape]);
    }
  }

  return lists;
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

export const getDistinguishingFields = <T>(
  shape: Shape<T>
): string[] => {
  const strokeFields = ["strokeStyle", "strokeDasharray"];
  const arrowFields = ["startArrowhead",  "endArrowhead", "flipStartArrowhead"];
  const stringFields = ["string", "fontSize"];

  switch (shape.shapeType) {
    case "Circle":
      return [...strokeFields];

    case "Ellipse":
      return [...strokeFields];

    case "Line":
      return ["strokeLinecap", ...arrowFields, ...strokeFields];

    case "Equation":
      return [...stringFields];

    case "Image":
      return ["href", "preserveAspectRatio"];

    case "Polygon":
      return [...strokeFields];

    case "Polyline":
      return ["strokeLinecap", ...strokeFields];

    case "Rectangle":
      return [...strokeFields];

    case "Path":
      return [...strokeFields, ...arrowFields];

    case "Text":
      return [
        "visibility",
        "fontFamily",
        "fontSizeAdjust",
        "fontStretch",
        "fontStyle",
        "fontVariant",
        "fontWeight",
        "textAnchor",
        "lineHeight",
        "alignmentBaseline",
        "dominantBaseline",
        ...strokeFields,
        ...stringFields,
      ];

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

const getComparisonVector = (
  shape: Shape<number>
): number[] => {
  const fields = getComparableFields(shape);

  const arr = fields.flatMap((field) => {
    const value = shape[field] as Value<number>;
    switch (value.tag) {
      case "FloatV":
        return [value.contents];

      case "TupV":
      case "VectorV":
      case "ListV":
        return value.contents;

      case "PtListV":
        return value.contents.flat();

      default:
        throw new Error(`Unsupported value type in comparison vector: ${value.tag}`);
    }
  });

  if (shape.shapeType === "Path") {
    const vertices = getPathVertices(shape.d.contents);
    arr.push(...vertices.flat());
  }

  return arr;
}

export const findDiagramDistance = (
  shapes1: Shape<number>[],
  shapes2: Shape<number>[],
): number => {
  const ll1 = getComparableLists(shapes1);
  const ll2 = getComparableLists(shapes2);

  if (ll1.length !== ll2.length) {
    throw new Error(`Shapes have different number of comparable lists: ${ll1.length} vs ${ll2.length}`);
  }

  // sort second lists to be comparable with first
  for (let i = 0; i < ll1.length; i++) {
    let swapped = false;
    for (let j = i; j < ll2.length; j++) {
      // check if first element is compatible
      if (areComparable(ll1[i][0], ll2[j][0])) {
        // swap
        [ll2[i], ll2[j]] = [ll2[j], ll2[i]];
        swapped = true;
        break;
      }
    }

    if (!swapped) {
      throw new Error(`No compatible shape found for list ${i}`);
    }
  }

  const dist2Fn = (a: Shape<number>, b: Shape<number>): number => {
    const v1 = getComparisonVector(a);
    const v2 = getComparisonVector(b);
    if (v1.length !== v2.length) {
      throw new Error(`Comparison vectors have different lengths: ${v1.length} vs ${v2.length}`);
    }
    return v1.reduce((acc, x, i) => acc + (x - v2[i]) ** 2, 0);
  }

  const matchings: [Shape<number>, Shape<number>][] = [];
  for (let i = 0; i < ll1.length; i++) {
    const l1 = ll1[i];
    const l2 = ll2[i];
    if (l1.length !== l2.length) {
      throw new Error(`Comparable lists have different lengths: ${l1.length} vs ${l2.length}`);
    }

    // find optimal matching between two lists
    const matching = findOptimalMatching(
      new Set(l1),
      new Set(l2),
      dist2Fn
    );
    matchings.push(...matching);
  }

  // Calculate total square distance
  const dist2 = matchings.reduce((acc, [a, b]) => acc + dist2Fn(a, b), 0);
  return Math.sqrt(dist2);
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

export const generalizedVariance = <T>(
  data: T[],
  metric: (a: T, b: T) => number
): number => {
  const n = data.length;
  let runningMean = 0;

  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (i === j) continue; // Skip self-distance

      const d = metric(data[i], data[j]);
      runningMean += (d ** 2 - runningMean) / (i * n + j + 1);
    }
  }

  return runningMean / 2;
}

