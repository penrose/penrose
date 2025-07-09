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
import { entries, Trio } from "@penrose/examples";
import seedrandom from "seedrandom";

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

const arePathsCompatible = (
  pathA: string,
  pathB: string
): boolean => {
  const splitA = pathA.split(" ");
  const splitB = pathB.split(" ");

  if (splitA.length !== splitB.length) {
    return false;
  }

  for (let i = 0; i < splitA.length; i++) {
    if (!isNaN(Number.parseFloat(splitA[i]))
      && !isNaN(Number.parseFloat(splitB[i])))
      continue;

    if (splitA[i] !== splitB[i])
      return false;
  }

  return true;
}

const getVerticesFromPath = (dString) => {
  if (!dString || typeof dString !== 'string') {
    return [];
  }

  const vertices = [];
  let currentX = 0;
  let currentY = 0;
  let subpathStartX = 0;
  let subpathStartY = 0;

  // Parse the path string into commands and parameters
  function parsePath(d) {
    const commands = [];
    const commandRegex = /[MmLlHhVvCcSsQqTtAaZz]/g;
    const numberRegex = /-?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g;

    let match;
    let lastIndex = 0;

    while ((match = commandRegex.exec(d)) !== null) {
      const command = match[0];
      const start = lastIndex;
      const end = match.index;

      // Extract numbers between previous command and current command
      if (start < end) {
        const numberStr = d.substring(start, end);
        const numbers = [];
        let numberMatch;
        while ((numberMatch = numberRegex.exec(numberStr)) !== null) {
          numbers.push(parseFloat(numberMatch[0]));
        }
        if (commands.length > 0 && numbers.length > 0) {
          commands[commands.length - 1].params = numbers;
        }
      }

      commands.push({ command, params: [] });
      lastIndex = match.index + 1;
    }

    // Handle numbers after the last command
    if (lastIndex < d.length) {
      const numberStr = d.substring(lastIndex);
      const numbers = [];
      let numberMatch;
      while ((numberMatch = numberRegex.exec(numberStr)) !== null) {
        numbers.push(parseFloat(numberMatch[0]));
      }
      if (commands.length > 0 && numbers.length > 0) {
        commands[commands.length - 1].params = numbers;
      }
    }

    return commands;
  }

  function addVertex(x, y) {
    vertices.push({ x, y });
  }

  const commands = parsePath(dString);

  for (const { command, params } of commands) {
    const isRelative = command === command.toLowerCase();
    const cmd = command.toUpperCase();

    switch (cmd) {
      case 'M': // Move to
        for (let i = 0; i < params.length; i += 2) {
          const x = isRelative ? currentX + params[i] : params[i];
          const y = isRelative ? currentY + params[i + 1] : params[i + 1];

          currentX = x;
          currentY = y;

          if (i === 0) {
            // First M command starts a new subpath
            subpathStartX = x;
            subpathStartY = y;
          }

          addVertex(x, y);
        }
        break;

      case 'L': // Line to
        for (let i = 0; i < params.length; i += 2) {
          const x = isRelative ? currentX + params[i] : params[i];
          const y = isRelative ? currentY + params[i + 1] : params[i + 1];

          currentX = x;
          currentY = y;
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
        for (let i = 0; i < params.length; i += 4) {
          // Control point: (x2, y2), end point: (x, y)
          const x = isRelative ? currentX + params[i + 2] : params[i + 2];
          const y = isRelative ? currentY + params[i + 3] : params[i + 3];

          currentX = x;
          currentY = y;
          addVertex(x, y);
        }
        break;

      case 'Q': // Quadratic Bézier curve
        for (let i = 0; i < params.length; i += 4) {
          // Control point: (x1, y1), end point: (x, y)
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
        break;
    }
  }

  return vertices;
}

export const removeAttrs = (str: string, attrs: string[]) => {
  for (const attr of attrs) {
    str = str.replace(new RegExp(`${attr}="[^"]*"`, "g"), "");
  }
  return str;
}

const areElemsCompatible = (
  elem1: SVGElement,
  elem2: SVGElement
): string => {
  // get outer html but remove inner html and closing tag, if any
  let str1 = elem1.outerHTML.match(/<[^/](?:[^">]*(?:".*")*)*>/)!.at(0)!;
  let str2 = elem2.outerHTML.match(/<[^/](?:[^">]*(?:".*")*)*>/)!.at(0)!;

  if (elem1.tagName !== elem2.tagName) {
    return false;
  }

  if (elem1.tagName === "path") {
    return arePathsCompatible(
      (elem1 as SVGPathElement).getAttribute("d"),
      (elem2 as SVGPathElement).getAttribute("d"));
  }

  const toRemove = getRemovableFields(elem1);
  str1 = removeAttrs(str1, toRemove);
  str2 = removeAttrs(str2, toRemove);

  return str1 === str2;
}

const getRemovableFields = (elem: SVGElement): string[] => {
  switch (elem.tagName) {
    case "circle":
      return ["cx", "cy", "r", "fill", "stroke"];
    case "ellipse":
      return ["cx", "cy", "rx", "ry", "fill", "stroke"];
    case "line":
      return ["x1", "y1", "x2", "y2", "stroke"];
    case "path":
      return ["d", "fill", "stroke"];
    case "polygon":
      return ["points", "fill", "stroke"];
    case "polyline":
      return ["points", "stroke"];
    case "rect":
      return ["x", "y", "width", "height", "fill", "stroke"];
    case "text":
      return ["x", "y"];
    case "image":
      return ["width", "height"];
    default:
      return [];
  }
}

const getComparableFields = (elem: SVGElement): string[] => {
  switch (elem.tagName) {
    case "circle":
      return ["cx", "cy", "r"];
    case "ellipse":
      return ["cx", "cy", "rx", "ry"];
    case "line":
      return ["x1", "y1", "x2", "y2"];
    case "path":
      return ["d"];
    case "polygon":
      return ["points"];
    case "polyline":
      return ["points"];
    case "rect":
      return ["x", "y", "width", "height"];
    case "text":
      return ["x", "y"];
    case "image":
      return ["width", "height"];
    default:
      return [];
  }
}

const getComparablePathVector = (elem: SVGPathElement): number[] => {
  const d = elem.getAttribute("d");
  if (d === null) return [];

  const vertices = getVerticesFromPath(d);
  return vertices.flatMap(v => [v.x, v.y]);
}

const getComparableVector = (elem: SVGElement): number[] => {
  if (elem.tagName === "path") {
    return getComparablePathVector(elem as SVGPathElement);
  }

  const comparableFields = getComparableFields(elem);
  const vector: number[] = [];
  for (const field of comparableFields) {
    const value = elem.getAttribute(field);
    if (value !== null) {
      vector.push(parseFloat(value));
    } else {
      throw new Error(`Field ${field} not found in element ${elem.tagName}`);
    }
  }

  return vector;
}

const createComparableLists = (
  svg: SVGElement
): SVGElement[][] => {
  const elems = Array.from(svg.querySelectorAll("*"))
    .filter((elem) => elem instanceof SVGElement);

  const penroseElemTags = new Set<string>([
    "circle",
    "ellipse",
    "line",
    "path",
    "polygon",
    "polyline",
    "rect",
    "text",
    "image",
  ]);

  const comparableLists: SVGElement[][] = [];
  for (const elem of elems) {
    if (!penroseElemTags.has(elem.tagName))
      continue;

    let foundExisting = false;
    for (const l of comparableLists) {
      if (areElemsCompatible(elem, l[0])) {
        l.push(elem);
        foundExisting = true;
        break;
      }
    }

    if (!foundExisting) {
      comparableLists.push([elem]);
    }
  }

  return comparableLists;
}

export const findSvgDistance = (
  svg1: SVGElement,
  svg2: SVGElement,
): number => {
  console.log("finding lists")
  const comparableLists1 = createComparableLists(svg1);
  const comparableLists2 = createComparableLists(svg2);
  console.log("comparable lists found", comparableLists1.length, comparableLists2.length);

  if (comparableLists1.length !== comparableLists2.length) {
    throw new Error(`SVGs have different number of comparable lists: ${comparableLists1.length} vs ${comparableLists2.length}`);
  }

  // sort second lists to be comparable with first
  for (let i = 0; i < comparableLists1.length; i++) {
    for (let j = i; j < comparableLists2.length; j++) {
      // check if first element is compatible
      if (areElemsCompatible(comparableLists1[i][0], comparableLists2[j][0])) {
        // swap
        [comparableLists2[i], comparableLists2[j]] =
          [comparableLists2[j], comparableLists2[i]];
      }
    }
  }

  const matchings: [SVGElement, SVGElement][] = [];
  for (let i = 0; i < comparableLists1.length; i++) {
    const l1 = comparableLists1[i];
    const l2 = comparableLists2[i];
    if (l1.length !== l2.length) {
      throw new Error(`Comparable lists have different lengths: ${l1.length} vs ${l2.length}`);
    }
    // find optimal matching between two lists
    const matching = findOptimalMatching(
      new Set(l1),
      new Set(l2),
      (a, b) => {
        const v1 = getComparableVector(a);
        const v2 = getComparableVector(b);
        return Math.sqrt(v1.reduce((acc, x, i) => acc + (x - v2[i]) ** 2, 0));
      }
    );
    matchings.push(...matching);
  }

  // Calculate total distance
  return matchings.reduce((acc, [a, b]) => {
    const v1 = getComparableVector(a);
    const v2 = getComparableVector(b);
    return acc + Math.sqrt(v1.reduce((sum, x, i) => sum + (x - v2[i]) ** 2, 0));
  }, 0);
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

