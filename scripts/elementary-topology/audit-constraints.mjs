/**
 * Critical audit of the registered Elementary Topology programs, without changing
 * their sources or regenerating their SVGs. Requires existing core/Bloom/example
 * dist modules (`yarn nx run core:build`, Bloom tsc, examples tsc).
 *
 * node scripts/elementary-topology/audit-constraints.mjs
 * --ids=5.7,11.20 --modes=static,interactive-canonical,interactive-resampled
 * --max-steps=100 --optimization-ms=250 --out=tmp/constraint-smoke.json
 *
 * The process-local wrappers only observe expressions. They do not add terms,
 * alter arguments, resample, or render SVGs. Private emitted JS fields are used
 * deliberately; this script fails attribution checks if that architecture changes.
 */
import { JSDOM } from "jsdom";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import ts from "typescript";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const flags = new Map(
  process.argv.slice(2).map((arg) => {
    const [key, ...value] = arg.replace(/^--/, "").split("=");
    return [key, value.join("=")];
  }),
);
const maxSteps = Number(flags.get("max-steps") ?? 100);
const optimizationMs = Number(flags.get("optimization-ms") ?? 250);
assert(Number.isSafeInteger(maxSteps) && maxSteps >= 0);
assert(Number.isFinite(optimizationMs) && optimizationMs >= 0);
const modes = (
  flags.get("modes") ?? "static,interactive-canonical,interactive-resampled"
).split(",");
assert(
  modes.every((mode) =>
    ["static", "interactive-canonical", "interactive-resampled"].includes(mode),
  ),
);
if (flags.has("refresh-summary")) {
  const target = path.resolve(
    root,
    flags.get("out") ?? "docs/elementary-topology/constraint-audit.json",
  );
  const recorded = JSON.parse(await readFile(target, "utf8"));
  summarizeAudit(recorded);
  await recordBaselineSourceChecks(recorded);
  await writeFile(target, JSON.stringify(recorded, null, 2) + "\n");
  console.log(
    `Refreshed summaries from recorded measurements: ${path.relative(
      root,
      target,
    )}`,
  );
  process.exit(0);
}
const selected = flags.has("ids")
  ? new Set(flags.get("ids").split(","))
  : undefined;
const output = path.resolve(
  root,
  flags.get("out") ?? "docs/elementary-topology/constraint-audit.json",
);
const loadJSON = async (filename) =>
  JSON.parse(await readFile(path.join(root, filename), "utf8"));
const sourceInventory = await loadJSON("docs/elementary-topology/figures.json");
const originalInventory = await loadJSON(
  "docs/elementary-topology/original-illustrations.json",
);
const allPrograms = [
  ...sourceInventory.figures
    .filter((figure) => figure.status === "reviewed")
    .map((figure) => ({ ...figure, registry: "source" })),
  ...originalInventory.illustrations.map((figure) => ({
    ...figure,
    registry: "original",
  })),
];
const figures = allPrograms.filter(
  (figure) => !selected || selected.has(figure.id),
);
if (selected)
  for (const id of selected)
    assert(
      figures.some((figure) => figure.id === id),
      `Unknown id ${id}`,
    );
const relative = (filename) =>
  path.relative(root, filename).replaceAll(path.sep, "/");
const modulePath = (filename) =>
  filename.replace("/src/", "/dist/").replace(/\.tsx?$/, ".js");
const hashFile = async (filename) =>
  createHash("sha256")
    .update(await readFile(path.join(root, filename)))
    .digest("hex");
const git = (...args) =>
  execFileSync("git", args, { cwd: root, encoding: "utf8" }).trim();
const round = (value) =>
  Number.isFinite(value) ? Number(value.toPrecision(10)) : null;
const elapsed = (start) => round(performance.now() - start);

const browser = new JSDOM("<!doctype html><html><body></body></html>", {
  pretendToBeVisual: true,
});
Object.assign(globalThis, {
  window: browser.window,
  document: browser.window.document,
  XMLSerializer: browser.window.XMLSerializer,
  requestAnimationFrame: browser.window.requestAnimationFrame.bind(
    browser.window,
  ),
  cancelAnimationFrame: browser.window.cancelAnimationFrame.bind(
    browser.window,
  ),
});
const core = await import(
  pathToFileURL(path.join(root, "packages/core/dist/index.js")).href
);
const { Diagram } = await import(
  pathToFileURL(path.join(root, "packages/bloom/dist/core/diagram.js")).href
);
const { collectVars } = core;
assert(
  typeof collectVars === "function",
  "Build core first: collectVars is required",
);
const gitHeadAtStart = git("rev-parse", "HEAD");
const sourceChangesAtStart = git("status", "--short", "--untracked-files=no");
const originalFreeze = Object.freeze;
const originalCreate = Diagram.create;
let activeRun;
const observedBuilders = new WeakMap();
const creationRecords = new WeakMap();
const objectMethods = new Set(["interactiveLabels", "draggableGroup"]);

function caller() {
  return (
    new Error().stack
      ?.split("\n")
      .find((line) =>
        /packages\/(bloom|examples)\/dist\/(styles|examples|elementary-topology)\//.test(
          line,
        ),
      )
      ?.trim() ?? null
  );
}
function instrumentBuilder(builder, ctx) {
  let observation = observedBuilders.get(builder);
  if (!observation) {
    observation = {
      builder,
      phase: "authored",
      source: null,
      terms: [],
      inputOrigins: new Map(),
      dragGroups: [],
    };
    observedBuilders.set(builder, observation);
    activeRun.builders.push(observation);
    for (const input of builder.inputs)
      observation.inputOrigins.set(input.handle, "constructor");
    const sample = builder.samplingContext.makeInput;
    builder.samplingContext.makeInput = (...args) => {
      const variable = sample(...args);
      observation.inputOrigins.set(
        variable,
        observation.inputScope ?? "shape-default",
      );
      return variable;
    };
    for (const method of ["ensure", "encourage"]) {
      const original = builder[method];
      builder[method] = (...args) => {
        const result = original(...args);
        const expressions =
          method === "ensure" ? builder.constraints : builder.objectives;
        observation.terms.push({
          kind: method === "ensure" ? "constraint" : "objective",
          category: observation.phase,
          expression: expressions.at(-1),
          source: observation.phase === "authored" ? caller() : null,
        });
        return result;
      };
    }
    const originalInput = builder.input;
    builder.input = (...args) => {
      const prior = observation.inputScope;
      observation.inputScope = "authored-input";
      try {
        return originalInput(...args);
      } finally {
        observation.inputScope = prior;
      }
    };
    for (const method of objectMethods) {
      const original = builder[method];
      builder[method] = (...args) => {
        const priorPhase = observation.phase,
          priorScope = observation.inputScope;
        if (method === "interactiveLabels")
          observation.phase = "generic-interactive-label";
        else if (observation.phase !== "generic-interactive-label")
          observation.phase = "generic-interactive-object";
        observation.inputScope = observation.phase;
        if (method === "draggableGroup")
          observation.dragGroups.push({
            role:
              observation.phase === "generic-interactive-label"
                ? "label"
                : "object",
            handle: args[0].name,
            handleType: args[0].shapeType,
            companions: (args[1] ?? []).map((shape) => shape.name),
            jitter: args[2]?.jitter ?? 4,
            maxDistance: args[2]?.maxDistance ?? 16,
          });
        try {
          return original(...args);
        } finally {
          observation.phase = priorPhase;
          observation.inputScope = priorScope;
        }
      };
    }
    const onCanvas = builder.getOnCanvasConstraints;
    builder.getOnCanvasConstraints = (...args) => {
      const expressions = onCanvas(...args);
      observation.terms.push(
        ...expressions.map((expression) => ({
          kind: "constraint",
          category: "generic-on-canvas",
          expression,
          source: null,
        })),
      );
      return expressions;
    };
  }
  // Context method bindings were captured before this observation wrapper.
  ctx.ensure = builder.ensure;
  ctx.encourage = builder.encourage;
  ctx.input = builder.input;
  if (!observation.contexts) observation.contexts = new WeakSet();
  if (!observation.contexts.has(ctx)) {
    observation.contexts.add(ctx);
    observation.substance = ctx.substance;
    observation.queries = {
      entities: new Map(),
      facts: new Map(),
      tests: new Map(),
    };
    for (const [method, group] of [
      ["entities", "entities"],
      ["facts", "facts"],
      ["test", "tests"],
    ]) {
      const original = ctx[method];
      ctx[method] = (...args) => {
        const result = original(...args);
        const name = args[0].name;
        const entry = observation.queries[group].get(name) ?? {
          calls: 0,
          results: 0,
        };
        entry.calls++;
        entry.results += Array.isArray(result) ? result.length : Number(result);
        observation.queries[group].set(name, entry);
        return result;
      };
    }
  }
  return observation;
}
// Observe every Domain.style apply, including styles constructed on module load.
Object.freeze = (object) => {
  if (
    object &&
    typeof object === "object" &&
    object.domain?.definitions &&
    typeof object.apply === "function"
  ) {
    const originalApply = object.apply;
    const styleCaller = caller();
    object.apply = function (ctx) {
      if (!activeRun) return originalApply.call(this, ctx);
      const observation = instrumentBuilder(ctx.builder, ctx);
      activeRun.styles.push(styleCaller);
      return originalApply.call(this, ctx);
    };
  }
  return originalFreeze(object);
};
Diagram.create = async (data) => {
  if (!activeRun) return originalCreate(data);
  const observation = activeRun.builders.find(
    ({ builder }) =>
      builder.canvas === data.canvas && builder.variation === data.variation,
  );
  if (!observation)
    throw new Error(
      "Cannot attribute Diagram.create to an observed style builder",
    );
  activeRun.creation = { data, observation };
  const result = await originalCreate(data);
  creationRecords.set(result, { data, observation });
  return result;
};

const expressionTags = new Set([
  "Var",
  "Unary",
  "Binary",
  "Ternary",
  "Nary",
  "Index",
  "Member",
  "Call",
  "PolyRoots",
  "LitVec",
  "LitRec",
  "Comp",
  "Logic",
  "Not",
]);
function expressionsIn(value, result = []) {
  if (typeof value === "number") result.push(value);
  else if (value && typeof value === "object") {
    if (expressionTags.has(value.tag)) result.push(value);
    else if (Array.isArray(value))
      for (const child of value) expressionsIn(child, result);
    else
      for (const [key, child] of Object.entries(value))
        if (key !== "tag") expressionsIn(child, result);
  }
  return result;
}
const geometryFields = {
  Circle: ["center", "r"],
  Ellipse: ["center", "rx", "ry"],
  Rectangle: ["center", "width", "height", "rotation", "cornerRadius"],
  Line: ["start", "end"],
  Path: ["d"],
  Polygon: ["points"],
  Polyline: ["points"],
  Equation: ["center", "width", "height", "rotation"],
  Text: ["center", "width", "height", "rotation"],
  Image: ["center", "width", "height", "rotation"],
  Group: [],
};
function flattenShapes(shapes) {
  const seen = new Set();
  const visit = (shape) => {
    if (seen.has(shape)) return;
    seen.add(shape);
    if (shape.shapeType === "Group") {
      shape.shapes.contents.forEach(visit);
      if (shape.clipPath?.contents?.tag === "Clip")
        visit(shape.clipPath.contents.contents);
    }
  };
  shapes.forEach(visit);
  return [...seen];
}
function counts(values) {
  return Object.fromEntries(
    [
      ...values.reduce(
        (map, value) => map.set(value, (map.get(value) ?? 0) + 1),
        new Map(),
      ),
    ].sort(),
  );
}
function dependency(expression, data, mask) {
  const variables = collectVars([expression]);
  const optimized = data.inputs.reduce(
    (n, input, i) => n + Number(mask[i] && variables.has(input.handle)),
    0,
  );
  return {
    variables,
    type:
      optimized > 0
        ? "optimized-dependent"
        : variables.size
        ? "pinned-or-pending-only"
        : "literal-constant",
    optimized,
  };
}
function analyze(data, observation, diagram) {
  const state = diagram.state;
  assert(
    state && data.inputs.length === state.inputs.length,
    "Emitted private state layout changed",
  );
  const mask = [...state.constraintSets.values()][0].inputMask;
  const optimizedHandles = new Set(
    data.inputs.filter((_, i) => mask[i]).map((input) => input.handle),
  );
  const terms = [...observation.terms];
  for (const kind of ["constraint", "objective"]) {
    const expected = kind === "constraint" ? data.constraints : data.objectives;
    const actual = terms.filter((term) => term.kind === kind);
    assert.equal(actual.length, expected.length, `Unattributed ${kind} count`);
    const remaining = [...actual];
    for (const expression of expected) {
      const i = remaining.findIndex((term) => term.expression === expression);
      assert(i !== -1, `Unattributed ${kind} expression`);
      remaining.splice(i, 1);
    }
  }
  const summarized = {};
  const termDependencies = new Map();
  for (const term of terms) {
    const key = `${term.kind}:${term.category}`;
    const entry = (summarized[key] ??= {
      total: 0,
      optimizedDependent: 0,
      pinnedOrPendingOnly: 0,
      literalConstant: 0,
    });
    const dep = dependency(term.expression, data, mask);
    entry.total++;
    entry[
      dep.type === "optimized-dependent"
        ? "optimizedDependent"
        : dep.type === "pinned-or-pending-only"
        ? "pinnedOrPendingOnly"
        : "literalConstant"
    ]++;
    termDependencies.set(term, dep);
  }
  const shapes = flattenShapes(data.shapes);
  const geometricVars = new Set(),
    labelVars = new Set(),
    nonLabelVars = new Set();
  let fixedGeometryShapes = 0,
    optimizedLabelShapes = 0,
    optimizedOtherShapes = 0;
  for (const shape of shapes) {
    const vars = collectVars(
      (geometryFields[shape.shapeType] ?? []).flatMap((field) =>
        expressionsIn(shape[field]),
      ),
    );
    const optimized = [...vars].filter((v) => optimizedHandles.has(v));
    const label = ["Equation", "Text"].includes(shape.shapeType);
    for (const variable of vars) {
      geometricVars.add(variable);
      (label ? labelVars : nonLabelVars).add(variable);
    }
    if (optimized.length === 0 && shape.shapeType !== "Group")
      fixedGeometryShapes++;
    else if (label) optimizedLabelShapes++;
    else optimizedOtherShapes++;
  }
  const reachableEnergy = collectVars([
    ...data.constraints,
    ...data.objectives,
  ]);
  const countInputs = (predicate) =>
    data.inputs.reduce((n, input, i) => n + Number(predicate(input, i)), 0);
  const dragGroups = observation.dragGroups;
  const otherVariables = countInputs(
    (input, i) =>
      mask[i] && nonLabelVars.has(input.handle) && !labelVars.has(input.handle),
  );
  const eligible = countInputs((_, i) => mask[i]);
  const inputs = {
    registered: data.inputs.length,
    optimizerEligible: eligible,
    pinnedOrPending: data.inputs.length - eligible,
    energyReachableOptimized: countInputs(
      (input, i) => mask[i] && reachableEnergy.has(input.handle),
    ),
    geometryReachableOptimized: countInputs(
      (input, i) => mask[i] && geometricVars.has(input.handle),
    ),
    labelGeometryOptimized: countInputs(
      (input, i) => mask[i] && labelVars.has(input.handle),
    ),
    nonLabelGeometryOptimized: countInputs(
      (input, i) => mask[i] && nonLabelVars.has(input.handle),
    ),
    nonLabelExclusiveGeometryOptimized: otherVariables,
    unreachableOptimized: countInputs(
      (input, i) =>
        mask[i] &&
        !reachableEnergy.has(input.handle) &&
        !geometricVars.has(input.handle),
    ),
    origins: counts(
      data.inputs.map(
        (input) => observation.inputOrigins.get(input.handle) ?? "lasso-copy",
      ),
    ),
    optimizedGeometryOrigins: counts(
      data.inputs
        .filter((input, i) => mask[i] && geometricVars.has(input.handle))
        .map(
          (input) => observation.inputOrigins.get(input.handle) ?? "lasso-copy",
        ),
    ),
  };
  const queryMaps = Object.fromEntries(
    Object.entries(observation.queries).map(([kind, map]) => [
      kind,
      Object.fromEntries([...map].sort()),
    ]),
  );
  return {
    counts: {
      shapeTopLevel: data.shapes.length,
      shapeUniqueIncludingGroupsAndClips: shapes.length,
      shapesByType: counts(shapes.map((shape) => shape.shapeType)),
      fixedGeometryShapes,
      geometryCountExcludesStructuralGroups: true,
      optimizedLabelShapes,
      optimizedOtherShapes,
      dragHandles: data.draggingConstraints.size,
      labelDragGroups: dragGroups.filter((group) => group.role === "label")
        .length,
      objectDragGroups: dragGroups.filter((group) => group.role === "object")
        .length,
      constraintCount: data.constraints.length,
      objectiveCount: data.objectives.length,
      disabledLassoObjectives: data.lassoStrength ? 1 : 0,
    },
    inputs,
    terms: summarized,
    freedom: dragGroups.some((group) => group.role === "object")
      ? "rigid-object-translation-with-associated-labels"
      : inputs.geometryReachableOptimized > 0
      ? "label-placement-with-possible-knockout-companions"
      : "fixed-geometry",
    authoredTermSources: [
      ...new Set(
        terms
          .filter((term) => term.category === "authored")
          .map((term) => term.source),
      ),
    ],
    queries: queryMaps,
    substance: {
      entityCount: observation.substance.entities.length,
      propositionCount: observation.substance.propositions.length,
      predicates: counts(
        observation.substance.propositions.map((p) => p.predicate.name),
      ),
    },
    _terms: terms,
  };
}
function feasibility(diagram, data, terms) {
  try {
    const state = diagram.state,
      mask = [...state.constraintSets.values()][0];
    const gradient = new Float64Array(state.inputs.length);
    const evaluated = state.gradient(
      mask,
      Float64Array.from(state.varyingValues),
      1,
      gradient,
    );
    assert.equal(evaluated.constraints.length, data.constraints.length);
    const constraintTerms = terms.filter((term) => term.kind === "constraint");
    const remaining = [...constraintTerms];
    const categories = data.constraints.map((expression) => {
      const i = remaining.findIndex((term) => term.expression === expression);
      return remaining.splice(i, 1)[0].category;
    });
    const byCategory = {};
    for (let i = 0; i < evaluated.constraints.length; i++) {
      const value = evaluated.constraints[i],
        category = categories[i];
      const entry = (byCategory[category] ??= {
        total: 0,
        violatedAboveTolerance: 0,
        nonFinite: 0,
        maxPositiveResidual: 0,
      });
      entry.total++;
      if (!Number.isFinite(value)) entry.nonFinite++;
      else {
        entry.violatedAboveTolerance += Number(value > 1e-5);
        entry.maxPositiveResidual = Math.max(entry.maxPositiveResidual, value);
      }
    }
    const finite = evaluated.constraints.filter(Number.isFinite);
    return {
      status: "measured",
      tolerance: 1e-5,
      nonFiniteConstraints: evaluated.constraints.length - finite.length,
      violatedAboveTolerance: finite.filter((value) => value > 1e-5).length,
      maxPositiveResidual: round(Math.max(0, ...finite)),
      sumSquaredPositiveResiduals: round(
        finite.reduce((sum, value) => sum + Math.max(0, value) ** 2, 0),
      ),
      energyAtWeightOne: round(evaluated.phi),
      maskedGradientNorm: round(Math.hypot(...gradient)),
      byCategory: Object.fromEntries(
        Object.entries(byCategory).map(([key, value]) => [
          key,
          { ...value, maxPositiveResidual: round(value.maxPositiveResidual) },
        ]),
      ),
    };
  } catch (error) {
    return { status: "unavailable", error: String(error) };
  }
}

const sourceCache = new Map();
async function sourceAST(filename) {
  if (!sourceCache.has(filename)) {
    const text = await readFile(path.join(root, filename), "utf8");
    sourceCache.set(
      filename,
      ts.createSourceFile(
        filename,
        text,
        ts.ScriptTarget.Latest,
        true,
        filename.endsWith(".tsx") ? ts.ScriptKind.TSX : ts.ScriptKind.TS,
      ),
    );
  }
  return sourceCache.get(filename);
}
const span = (source, node) => [
  source.getLineAndCharacterOfPosition(node.getStart(source)).line + 1,
  source.getLineAndCharacterOfPosition(node.getEnd()).line + 1,
];
function declaration(source, name) {
  let found;
  function visit(node) {
    if (ts.isFunctionDeclaration(node) && node.name?.text === name)
      found = node;
    if (ts.isVariableDeclaration(node) && node.name.getText(source) === name)
      found = node;
    ts.forEachChild(node, visit);
  }
  visit(source);
  return found;
}
async function sourceEvidence(figure) {
  const source = await sourceAST(figure.implementation),
    decl = declaration(source, figure.buildFactory);
  assert(decl, `Missing build factory ${figure.buildFactory}`);
  const callable = ts.isVariableDeclaration(decl) ? decl.initializer : decl;
  assert(
    callable && callable.parameters,
    `Factory is not a direct callable: ${figure.buildFactory}`,
  );
  const last = callable.parameters.at(-1),
    lastName = last?.name.getText(source);
  const optionParameter = /renderOptions|options/.test(lastName ?? "")
    ? callable.parameters.length - 1
    : null;
  assert(
    optionParameter !== null,
    `Cannot locate trailing figure options in ${figure.buildFactory}`,
  );
  return {
    factoryLines: span(source, decl),
    optionParameter,
    style: figure.style,
    substanceModule: figure.substanceModule ?? figure.implementation,
    substanceFactory: figure.substanceFactory,
  };
}
async function styleEvidence(filename) {
  const source = await sourceAST(filename);
  const calls = [],
    imports = [],
    literalPathCalls = [];
  let numericLiterals = 0,
    throwStatements = 0;
  function visit(node) {
    if (ts.isNumericLiteral(node)) numericLiterals++;
    if (ts.isThrowStatement(node)) throwStatements++;
    if (ts.isImportDeclaration(node)) imports.push(node.moduleSpecifier.text);
    if (ts.isCallExpression(node)) {
      const text = node.expression.getText(source);
      const finalName = text.split(".").at(-1);
      if (
        [
          "ensure",
          "encourage",
          "input",
          "bindToInput",
          "draggableGroup",
          "interactiveLabels",
          "test",
          "facts",
          "entities",
        ].includes(finalName)
      )
        calls.push({ expression: text, line: span(source, node)[0] });
      if (
        /[Dd]ata|pageChart|outline|[Pp]ath/.test(finalName) &&
        node.arguments.some((arg) => ts.isArrayLiteralExpression(arg))
      )
        literalPathCalls.push({
          expression: text,
          line: span(source, node)[0],
        });
    }
    ts.forEachChild(node, visit);
  }
  visit(source);
  return {
    filename,
    sha256: await hashFile(filename),
    numericLiteralCount: numericLiterals,
    throwStatementCount: throwStatements,
    optimizationCalls: calls.filter((call) =>
      /(?:^|\.)(ensure|encourage|input|bindToInput)$/.test(call.expression),
    ),
    interactionCalls: calls.filter((call) =>
      /\.(draggableGroup|interactiveLabels)$/.test(call.expression),
    ),
    queryCalls: calls.filter((call) =>
      /\.(test|facts|entities)$/.test(call.expression),
    ),
    literalCoordinateCallSites: literalPathCalls,
    imports,
    interpretation:
      "Numeric literals and validation throws are evidence to inspect, not an automatic overfitting score. Boolean tests/facts do not themselves add optimization terms.",
  };
}

const evidence = new Map();
for (const figure of figures)
  evidence.set(figure.id, await sourceEvidence(figure));
const styles = await Promise.all(
  [...new Set(figures.map((figure) => figure.style))].sort().map(styleEvidence),
);
const fingerprints = await Promise.all(
  [
    ...new Set([
      "packages/core/dist/index.js",
      "packages/core/dist/engine/Autodiff.js",
      "packages/core/dist/lib/Constraints.js",
      "packages/bloom/dist/core/builder.js",
      "packages/bloom/dist/core/diagram.js",
      "packages/bloom/dist/core/program.js",
      ...figures.map((figure) => modulePath(figure.implementation)),
      ...figures.map((figure) => modulePath(figure.style)),
    ]),
  ]
    .sort()
    .map(async (filename) => ({ filename, sha256: await hashFile(filename) })),
);
// Cache all relevant ESM dependencies before any measurements. Concurrent source
// edits/builds elsewhere cannot change this process's already loaded module graph.
const programs = new Map();
for (const figure of figures) {
  const filename = modulePath(figure.implementation);
  if (!programs.has(filename))
    programs.set(
      filename,
      await import(pathToFileURL(path.join(root, filename)).href),
    );
}
const preloadFingerprintChecks = await Promise.all(
  fingerprints.map(async ({ filename, sha256 }) => ({
    filename,
    unchanged: (await hashFile(filename)) === sha256,
  })),
);
assert(
  preloadFingerprintChecks.every((check) => check.unchanged),
  "Compiled modules changed during preload; rerun to pin a coherent build",
);

// Positive controls prevent 'no authored terms' from being an instrumentation
// blind spot. The second constraint is deliberately impossible and constant.
const { domain, diagram, canvas } = await import(
  pathToFileURL(path.join(root, "packages/bloom/dist/core/program.js")).href
).then(async (program) => ({
  ...program,
  canvas: (
    await import(
      pathToFileURL(path.join(root, "packages/bloom/dist/core/utils.js")).href
    )
  ).canvas,
}));
assert(typeof canvas === "function");
const d = domain("ConstraintAuditCalibration"),
  T = d.type("Thing"),
  dom = d.make({ Thing: T }),
  s = dom.substance();
s.Thing({ label: "calibration" });
const sub = s.make();
activeRun = { builders: [], styles: [], creation: null };
const calibration = await diagram({
  sub,
  canvas: canvas(100, 100),
  variation: "constraint-audit-positive-control",
  sty: dom.style((ctx) => {
    const x = ctx.input({ init: 0 });
    ctx.circle({
      name: "calibration",
      center: [x, 0],
      r: 5,
      fillColor: [0, 0, 0, 1],
      strokeWidth: 0,
    });
    ctx.ensure(core.sub(10, x));
    ctx.ensure(1);
    ctx.encourage(core.mul(core.sub(x, 20), core.sub(x, 20)));
  }),
});
const calRecord = creationRecords.get(calibration),
  calMetrics = analyze(calRecord.data, calRecord.observation, calibration);
assert.equal(calMetrics.terms["constraint:authored"].total, 2);
assert.equal(calMetrics.terms["constraint:authored"].optimizedDependent, 1);
assert.equal(calMetrics.terms["constraint:authored"].literalConstant, 1);
assert.equal(calMetrics.terms["objective:authored"].total, 1);
assert.equal(calMetrics.inputs.geometryReachableOptimized, 1);
const calInitial = feasibility(calibration, calRecord.data, calMetrics._terms);
assert.equal(calInitial.status, "measured");
assert(calInitial.violatedAboveTolerance >= 2);
let calSteps = 0;
while (calSteps < 100) {
  calSteps++;
  if (!(await calibration.optimizationStep())) break;
}
const calFinal = feasibility(calibration, calRecord.data, calMetrics._terms);
assert(
  calFinal.violatedAboveTolerance >= 1,
  "Impossible constant residual must stay visible",
);
const selfChecks = {
  status: "passed",
  checks: [
    "authored optimized constraint attributed",
    "authored constant constraint attributed",
    "authored objective attributed",
    "exact geometry dependency found",
    "unresolvable positive constant residual retained",
  ],
  initial: calInitial,
  final: calFinal,
  optimizerStatus: calibration.state.params.optStatus,
  steps: calSteps,
};
calibration.discard();
activeRun = undefined;
const runs = [];
for (const figure of figures)
  for (const mode of modes) {
    const start = performance.now();
    const run = {
      id: figure.id,
      registry: figure.registry,
      mode,
      styleFamily: figure.style,
      buildFactory: figure.buildFactory,
      buildArguments: figure.buildArguments ?? [],
      source: evidence.get(figure.id),
    };
    activeRun = { builders: [], styles: [], creation: null };
    let drawing;
    try {
      const program = programs.get(modulePath(figure.implementation));
      assert(typeof program[figure.buildFactory] === "function");
      const args = [...(figure.buildArguments ?? [])];
      if (mode !== "static") {
        while (args.length < run.source.optionParameter) args.push(undefined);
        args.push({
          variation: `constraint-audit-${figure.id}`,
          interactive: { jitter: mode === "interactive-canonical" ? 0 : 4 },
        });
      }
      const buildStart = performance.now();
      drawing = await program[figure.buildFactory](...args);
      run.buildMilliseconds = elapsed(buildStart);
      const record = creationRecords.get(drawing);
      assert(record, "Factory did not produce an instrumented diagram");
      const metrics = analyze(record.data, record.observation, drawing);
      const terms = metrics._terms;
      delete metrics._terms;
      Object.assign(run, metrics);
      run.runtimeStyleCreators = activeRun.styles;
      run.initial = feasibility(drawing, record.data, terms);
      let steps = 0,
        stopped = false;
      const optimizeStart = performance.now();
      while (
        steps < maxSteps &&
        performance.now() - optimizeStart < optimizationMs
      ) {
        steps++;
        if (!(await drawing.optimizationStep())) {
          stopped = true;
          break;
        }
      }
      run.optimization = {
        steps,
        milliseconds: elapsed(optimizeStart),
        stopped,
        status: drawing.state.params.optStatus,
        budgetLimited: !stopped,
        maxSteps,
        millisecondsBudget: optimizationMs,
      };
      run.final = feasibility(drawing, record.data, terms);
      run.status = "measured";
    } catch (error) {
      run.status = "failed";
      run.error = error instanceof Error ? error.stack : String(error);
    } finally {
      drawing?.discard();
      activeRun = undefined;
    }
    run.totalMilliseconds = elapsed(start);
    runs.push(run);
    console.log(
      `${runs.length}/${figures.length * modes.length} ${figure.id} ${mode}: ${
        run.status
      }${
        run.status === "measured"
          ? `; active geometry=${
              run.inputs.geometryReachableOptimized
            }, authored terms=${run.terms["constraint:authored"]?.total ?? 0}/${
              run.terms["objective:authored"]?.total ?? 0
            }, final violated=${run.final.violatedAboveTolerance ?? "unknown"}`
          : `; ${run.error.split("\n")[0]}`
      }`,
    );
  }
Object.freeze = originalFreeze;
Diagram.create = originalCreate;
const groups = styles.map((style) => {
  const familyRuns = runs.filter((run) => run.styleFamily === style.filename);
  return {
    style: style.filename,
    programIds: [...new Set(familyRuns.map((run) => run.id))],
    modes: Object.fromEntries(
      modes.map((mode) => {
        const modeRuns = familyRuns.filter((run) => run.mode === mode),
          measured = modeRuns.filter((run) => run.status === "measured");
        const sum = (getter) =>
          measured.reduce((total, run) => total + getter(run), 0);
        return [
          mode,
          {
            attempted: modeRuns.length,
            measured: measured.length,
            failed: modeRuns.length - measured.length,
            shapes: sum((run) => run.counts.shapeUniqueIncludingGroupsAndClips),
            optimizedGeometryInputs: sum(
              (run) => run.inputs.geometryReachableOptimized,
            ),
            authoredConstraints: sum(
              (run) => run.terms["constraint:authored"]?.total ?? 0,
            ),
            authoredObjectives: sum(
              (run) => run.terms["objective:authored"]?.total ?? 0,
            ),
            genericConstraints: sum(
              (run) =>
                run.counts.constraintCount -
                (run.terms["constraint:authored"]?.total ?? 0),
            ),
            genericObjectives: sum(
              (run) =>
                run.counts.objectiveCount -
                (run.terms["objective:authored"]?.total ?? 0),
            ),
            freedom: counts(measured.map((run) => run.freedom)),
            stoppedWithinBudget: measured.filter(
              (run) => run.optimization.stopped,
            ).length,
            finalResidualsAboveTolerance: measured
              .filter((run) => run.final.violatedAboveTolerance > 0)
              .map((run) => run.id),
          },
        ];
      }),
    ),
    sourceEvidence: style,
  };
});
const result = {
  schemaVersion: 1,
  generatedAt: new Date().toISOString(),
  gitHead: gitHeadAtStart,
  baseline: "baf76d69f3b200a31b4d9a0dc82d93bbbd2d168f",
  sourceTrackedChangesAtStart: sourceChangesAtStart,
  command: `node scripts/elementary-topology/audit-constraints.mjs${
    process.argv.slice(2).length ? " " + process.argv.slice(2).join(" ") : ""
  }`,
  requested: {
    allRegisteredPrograms: allPrograms.length,
    sourceFigures: allPrograms.filter((p) => p.registry === "source").length,
    originals: allPrograms.filter((p) => p.registry === "original").length,
    selectedPrograms: figures.length,
    modes,
    maxSteps,
    optimizationMs,
  },
  coverage: {
    attemptedRuns: runs.length,
    measuredRuns: runs.filter((r) => r.status === "measured").length,
    failedRuns: runs.filter((r) => r.status === "failed").length,
    uniqueProgramsMeasuredAllRequestedModes: figures.filter((figure) =>
      modes.every((mode) =>
        runs.some(
          (run) =>
            run.id === figure.id &&
            run.mode === mode &&
            run.status === "measured",
        ),
      ),
    ).length,
  },
  methodology: [
    "Loads the same compiled leaf factories and registered arguments as render-book.mjs. Static defaults are unchanged. Interactive-canonical uses the reader's initial jitter=0; interactive-resampled uses its Sample action jitter=4 with a reproducible audit seed.",
    "Process-local Object.freeze observes Domain.style apply; builder wrappers record every ensure/encourage/input, generic interactive helpers, and on-canvas terms. Diagram.create expressions are checked exactly against those observations before metrics are accepted.",
    "Counts optimizer-eligible inputs separately from reachable inputs. Sampled defaults discarded by literal props remain registered but do not control geometry; these are reported as unreachable rather than as meaningful optimization freedom. literalConstant means no AD variables, including computed expressions over numeric values.",
    "Shapes include unique nested groups and clip shapes. Geometry fields exclude paint and stroke parameters. Label placement and rigid translation of an already fixed object do not count as semantic geometric construction.",
    "Boolean ctx.test checks and ctx.facts tuple queries are recorded separately. They query declared facts and may validate/select an illustration, but do not themselves add optimizer constraints or prove the asserted theorem.",
    "Feasibility comes from the compiled gradient's raw inequality residuals at weight=1. Positive values above 1e-5 are violations. Optimizer convergence is recorded independently and does not imply feasibility. Evaluation and optimization use no SVG rendering.",
    "Lasso continuity objectives appended by Diagram.makeState are separately counted as initially disabled. They are not authored style objectives. Optimization is bounded by step/time budgets and limits are explicit.",
    "Source AST metrics cover each entire registered style file; these include helper definitions and cannot prove generalizability. Numeric constants alone are not an overfitting score. Human source review must distinguish analytic geometry from fixed source contours.",
  ],
  limitations: [
    "Node/JSDOM provides the same mathematical build and constraint graph, not an actual browser gesture or accessibility audit.",
    "The audit uses emitted private JS fields for instrumentation. Attribution assertions fail rather than silently accepting missing terms after architecture changes.",
    "No figure SVG is regenerated and no source/core/style program is modified. Fingerprints record primary runtime modules and each registered compiled style/factory; the full ESM graph was cached before measurements.",
    "Finite residuals and graph dependencies do not prove semantic correctness, visual fidelity, theorem validity, or reuse on unseen Substance programs.",
    "Unreachable optimized input counts mean unreachable from energy and geometric fields; paint/stroke fields are outside that measure. FixedGeometryShapes counts direct geometric leaf fields, excluding structural groups.",
    "A generic canvas violation can arise from intentional source clipping or a conservative control-point bounding box. It is still a violation of the registered term and does not by itself prove incorrect mathematical content.",
    "Default registered programs plus two interaction states are tested. Alternate domains/programs, changed entity counts and parameter sweeps are outside this invocation.",
  ],
  selfChecks,
  buildFingerprints: fingerprints,
  families: groups,
  runs,
};
summarizeAudit(result);
await recordBaselineSourceChecks(result);
await mkdir(path.dirname(output), { recursive: true });
await writeFile(output, JSON.stringify(result, null, 2) + "\n");
console.log(
  `Saved ${relative(output)}: ${result.coverage.measuredRuns}/${
    result.coverage.attemptedRuns
  } runs measured.`,
);
browser.window.close();
process.exit(result.coverage.failedRuns ? 1 : 0);

/** Reviewed source mechanisms, not a score derived from literal counts. */
function sourceMechanismReviews() {
  const table = `
baire-category | fixed-source-schematic | 34-70,84-135 | Three selected nested balls or one dense-intersection sketch use fixed radii/positions; several point roles are selected by literal labels.
bounded-sets | fixed-source-schematic | 27-46,52-99 | Checks a centered bounding square but draws a fixed page-coordinate box/blob independently of its radius.
closed-interval | analytic-data-driven | 18-55 | Supports one closed interval on the absolute-value line and an exterior neighborhood whose radius equals its distance to that interval.
components | mixed | 19-31,40-72 | Computes eight reciprocal-circle radii; two component lines and coordinate legends use fixed source positions.
connected-closures | mixed | 40-64,74-128 | Samples one reciprocal polar spiral through a source-tuned nonlinear radial chart, with an explicit alternate chart option.
connected-products | analytic-data-driven | 37-76,81-107 | Supports a two-dimensional coordinate-box witness with horizontal/vertical fibers; does not construct arbitrary connected products.
connectedness | mixed | 82-90,163-222,235-268,308-349 | Computes polygonal routes and sine graphs, but interval separation cut=0.71 and one enclosing region is a fixed contour.
contraction-iterates | analytic-data-driven | 19-51,66-102,135-150 | One affine real contraction with explicit consecutive iterates/fixed point; does not draw arbitrary metric-space contractions.
convergence | mixed | 20-41,71-88,102-173 | Computes supplied interval partitions; unseparable-neighborhood branch uses fixed blobs and point positions.
convexity | mixed | 13-38,91-126,132-176 | Supports a 3D open-box segment or polygonal reachability witness with a separately authored enclosing region.
countable-enumeration | analytic-data-driven | 21-55,84-114 | Requires a consecutive rectangular finite window and a fixed diagonal enumeration algorithm.
covering-properties | mixed | 29-89,99-153,186-207,306-405 | Finite-reach branch uses interval coordinates; separated-cover/compact-Hausdorff branches use source-fixed grids/contours.
decimal-tables | analytic-data-driven | 39-74,92-106 | Renders explicit consecutive zero/one decimal prefixes; does not derive infinite enumeration or uncountability.
derived-sets | analytic-data-driven | 28-79,166-204 | Prescribed disk/interval configurations must agree with explicit bounds; does not compute arbitrary derived sets.
directed-intersections | fixed-source-schematic | 12-71,77-155 | A directed-family intersection/proposed split witness is assigned fixed source contours and outside-selection positions.
dyadic-neighborhoods | fixed-source-schematic | 19-84,95-102,239-258 | Separation/refinement facts determine roles/labels, while nested contours are fixed.
endpoint-identification | analytic-data-driven | 63-113,151-181,215-218 | Supports a quotient identifying two interval endpoints, circle realization, and one interior representative.
function-neighborhoods | mixed | 22-38,53-68,90-110 | Collar radius/intervals are data-driven; graph is a Style sampler with an authored representative default formula.
function-sequences | analytic-data-driven | 29-48,70-74,123-147 | Supports power functions on [0,1] and prescribed endpoint-limit collar; not arbitrary sequences.
fundamental-groups | fixed-source-schematic | 27-99,155-238,242-280 | Fixed sphere/torus/basepoint-change compositions; torus accepts exactly two unit-winding factors but paths are literal.
group-kernels | analytic-data-driven | 23-30,177-203,209-248 | Finite cyclic homomorphism with explicit residue elements/images/kernel, displayed in a prescribed column layout.
group-tables | analytic-data-driven | 30-79,82-107 | Requires a complete validated finite group table of 1-16 elements with explicit identity/inverse facts.
homotopies | mixed | 224-242,279-325,504-568,680-701 | Computes radial disks and slice heights; generic target/image contours are canned and require a supported slice structure.
homotopy-constructions | mixed | 24-46,105-159,262-347,374-454 | Computes parabolic arcs and contract/slide images; pasting/extension target geometry and sample times are prescribed.
homotopy-equivalence | fixed-source-schematic | 12-33,38-84 | Contractible/singleton equivalence facts select the same fixed blob, points, and arrows.
identification-sources | mixed | 49-81,87-129,136-169,171-198 | Projects rectangle/disk/polygon data; integer-difference-plane branch uses a fixed viewport.
limit-uniqueness | mixed | 18-84,86-98,127-134 | Half-distance balls follow proposed-limit coordinates; displayed sequence prefix uses fixed illustrative points.
local-compactness | fixed-source-schematic | 50-109,138-201,235-277 | Compact-neighborhood/irrational-cut/locally-closed facts are checked but all three views use fixed page-coordinate drawings.
loop-retracing | analytic-data-driven | 25-79,80-121 | A nonconstant trigonometric loop and its true inverse-cancellation family are displayed as four slices or one chosen slice.
loops | mixed | 111-155,164-194,243-273,288-477,480-520 | Trigonometric families are computed; basic loops, concatenation laws, and inverse-circle compositions use fixed geometry.
lower-limit-topology | analytic-data-driven | 15-54,70-96 | One half-open product rectangle and optionally the x+y=0 antidiagonal singleton-intersection case.
metric-comparison | analytic-data-driven | 36-84,87-122 | One concentric Euclidean-taxicab-Euclidean chain with numerically valid radii.
metric-completeness | mixed | 18-40,63-115,150-188 | Computes selected intervals, terms, and diameters; earlier bisection endpoint marks/legends use schematic fixed positions.
metric-continuity | fixed-source-schematic | 89-91,120-158,160-205 | Requires neighborhood-image inclusion and a mapped witness; actual radii/points do not determine blob/ball geometry.
metric-covers | analytic-data-driven | 35-80,113-153 | One compact real interval with at most four visible cover balls; proof points selected by labels x,z.
metric-neighborhoods | analytic-data-driven | 30-54,75-109 | Euclidean/taxicab/supremum/discrete balls in one plane; discrete radii above 1 are rejected.
neighborhood-inclusion | analytic-data-driven | 19-52,57-73 | Euclidean two-ball inclusion with exact radius rho-distance; not arbitrary neighborhoods.
oscillating-sine-curve | analytic-data-driven | 112-153,181-190 | The positive oscillating-sine branch plus isolated origin in a finite normalized viewport.
path-extensions | mixed | 14-28,55-80,83-128,152-170 | A two-edge extension on the unit square uses Style-supplied paths/displacement; default boundaries are authored Beziers.
planar-graph-spaces | fixed-source-schematic | 93-227,275-303 | Exactly eleven exercise spaces/nine distinct glyphs with a finite catalog of authored contours.
plane-curves | analytic-data-driven | 22-46,103-125,147-169 | Explicit supported piecewise curve segments in one based loop family/closed disk; topology facts do not infer those coordinates.
point-set-topology | mixed | 176-242,268-330,331-421,427 | Disk/reciprocal-set views are computed; abstract separations use fixed source contours/placements.
product-surfaces | analytic-data-driven | 63-82,130-150,206-257 | Normalized I×C or I^3 realization with Style dimensions/camera; interval values do not determine physical view dimensions.
product-topology | analytic-data-driven | 38-89,234-253,290-300 | One coordinate strip, two-strip rectangle, or coordinate embedding with a representative mapped point.
projection-preimage | analytic-data-driven | 18-39,53-75 | First-coordinate projection from Euclidean plane to absolute-value line; infinite strip is viewport-clipped.
quotient-spaces | mixed | 12-47,47-103,121-158,229-239 | Collapse views use fixed contours; circle product uses a procedural torus and a widened fiber glyph.
separated-disks | fixed-source-schematic | 16-35,50-61,71-89 | Rejects all disk pairs except unit disks centered at (0,0),(2,0), then draws that fixed source composition.
separation-axioms | mixed | 16-99,103-157,192-246,252-291,354-359 | Separation/closure contours are fixed; reciprocal counterexample uses a source-tuned piecewise-linear display map.
sequence-convergence | mixed | 57-75,91-129,169-225,249-251 | Prescribed finite-tail witnesses; term positions come from Style samplers with authored prefix/default tail.
subspaces | analytic-data-driven | 21-48,64-96,98-144 | Horizontal affine-line subspace with a disk neighborhood/off-line witness; not arbitrary subspaces.
topological-projections | analytic-data-driven | 29-80,116-156 | Central segment or triangle-to-circle radial projection with one explicit mapped point.
topology-bases | analytic-data-driven | 197-265,273-322,358-405 | Supported interval/ray intersections, four square-bounding half-planes, or exactly two disks with an intervening triangle.
`;
  return Object.fromEntries(
    table
      .trim()
      .split("\n")
      .map((line) => {
        const [basename, mechanism, lines, reuseLimit] = line.split(" | ");
        return [
          `${basename}.tsx`,
          {
            mechanism,
            evidenceLines: lines.split(","),
            reuseLimit,
            scope:
              "Human-reviewed whole-module mechanism; mixed modules may differ by figure branch. No optimizer constraint inference is implied by analytic/data-driven.",
          },
        ];
      }),
  );
}
function summarizeAudit(result) {
  for (const note of [
    "Unreachable optimized input counts mean unreachable from energy and geometric fields; paint/stroke fields are outside that measure. FixedGeometryShapes counts direct geometric leaf fields, excluding structural groups.",
    "literalConstant means no AD variables, including computed expressions over numeric values; it does not mean the final geometry was necessarily entered as a numeric literal.",
    "A generic canvas violation can arise from intentional source clipping or a conservative control-point bounding box. It is still a violation of the registered term and does not by itself prove incorrect mathematical content.",
  ])
    if (!result.limitations.includes(note)) result.limitations.push(note);
  const reviews = sourceMechanismReviews();
  for (const run of result.runs.filter((r) => r.status === "measured")) {
    if (!run.counts.geometryCountExcludesStructuralGroups) {
      run.counts.fixedGeometryShapes -= run.counts.shapesByType.Group ?? 0;
      run.counts.geometryCountExcludesStructuralGroups = true;
    }
    run.freedom =
      run.counts.objectDragGroups > 0
        ? "rigid-object-translation-with-associated-labels"
        : run.inputs.geometryReachableOptimized > 0
        ? "label-placement-with-possible-knockout-companions"
        : "fixed-geometry";
  }
  for (const family of result.families) {
    const review = reviews[path.basename(family.style)];
    assert(review, `Missing human source review for ${family.style}`);
    family.sourceMechanismReview = review;
    for (const [mode, summary] of Object.entries(family.modes))
      summary.freedom = counts(
        result.runs
          .filter(
            (r) =>
              r.status === "measured" &&
              r.styleFamily === family.style &&
              r.mode === mode,
          )
          .map((r) => r.freedom),
      );
  }
  result.summary = {
    modes: Object.fromEntries(
      result.requested.modes.map((mode) => {
        const runs = result.runs.filter(
          (r) => r.mode === mode && r.status === "measured",
        );
        const sum = (getter) => runs.reduce((n, r) => n + getter(r), 0);
        const terms = {};
        for (const run of runs)
          for (const [category, counts] of Object.entries(run.terms)) {
            const total = (terms[category] ??= {
              total: 0,
              optimizedDependent: 0,
              pinnedOrPendingOnly: 0,
              literalConstant: 0,
            });
            for (const key of Object.keys(total)) total[key] += counts[key];
          }
        return [
          mode,
          {
            programsMeasured: runs.length,
            authoredConstraintPrograms: runs.filter(
              (r) => (r.terms["constraint:authored"]?.total ?? 0) > 0,
            ).length,
            authoredObjectivePrograms: runs.filter(
              (r) => (r.terms["objective:authored"]?.total ?? 0) > 0,
            ).length,
            constraints: sum((r) => r.counts.constraintCount),
            objectives: sum((r) => r.counts.objectiveCount),
            terms,
            registeredInputs: sum((r) => r.inputs.registered),
            optimizerEligibleInputs: sum((r) => r.inputs.optimizerEligible),
            optimizedGeometryInputs: sum(
              (r) => r.inputs.geometryReachableOptimized,
            ),
            unreachableOptimizedInputs: sum(
              (r) => r.inputs.unreachableOptimized,
            ),
            freedom: counts(runs.map((r) => r.freedom)),
            objectTranslationPrograms: runs
              .filter((r) => r.counts.objectDragGroups > 0)
              .map((r) => r.id),
            finalViolatingPrograms: runs
              .filter((r) => r.final.violatedAboveTolerance > 0)
              .map((r) => ({
                id: r.id,
                violations: r.final.violatedAboveTolerance,
                maxPositiveResidual: r.final.maxPositiveResidual,
                optimizerStatus: r.optimization.status,
                byCategory: r.final.byCategory,
              })),
            feasibilityUnavailable: runs
              .filter(
                (r) =>
                  r.initial.status !== "measured" ||
                  r.final.status !== "measured",
              )
              .map((r) => r.id),
            nonFiniteConstraintValues: sum(
              (r) => r.final.nonFiniteConstraints ?? 0,
            ),
            optimizationBudgetLimited: runs
              .filter((r) => r.optimization.budgetLimited)
              .map((r) => r.id),
          },
        ];
      }),
    ),
    sourceMechanismsByFamily: counts(
      result.families.map((f) => f.sourceMechanismReview.mechanism),
    ),
    optimizationCallsInRegisteredStyleAST: result.families.reduce(
      (n, f) => n + f.sourceEvidence.optimizationCalls.length,
      0,
    ),
    criticalFindings: [
      "All registered static programs use fixed numeric geometry at assembly. Analytic/procedural formulas are present, but none of the 110 static diagrams constructs geometry by solving authored constraints.",
      "Every observed registered constraint/objective belongs to generic canvas, label, or rigid-object interaction machinery. Boolean semantic assertions are validation or branch selection, not optimizer constraints.",
      "Many registered sampled defaults are overwritten by fixed props yet remain in the input list. Counting eligible/registered inputs alone materially overstates optimizer use.",
      "Interactive translation of already drawn geometry demonstrates gesture support, not general mathematical layout or synthesis from relations.",
      "Several diagrams retain violated constant/pinned canvas terms after EPConverged. Mathematical construction, optimization completion, generic feasibility, and source fidelity are separate claims.",
      "Reusable declarations/analytic helpers coexist with source-specific compositions. Multiple styles accept tightly restricted entity counts, special coordinates, or literal label roles; changing arbitrary Substance does not imply a correct new illustration.",
    ],
  };
  result.manualSourceReview = {
    baseline: "baf76d69f3b200a31b4d9a0dc82d93bbbd2d168f",
    styleFilesReviewed: result.families.length,
    classificationMeaning:
      "Mechanism classes describe procedural vs source-coordinate geometry. They do not imply unconstrained domain coverage or a numeric quality/overfitting score.",
    nonRegisteredConstraintExample: {
      filename: "packages/bloom/src/styles/set-theory.tsx",
      line: 53,
      note: "Membership/subset/disjointness/intersection constraints exist in this separate style, but none of the 110 registry entries selects it.",
    },
    overfitExamples: [
      {
        ids: ["10.5", "10.6"],
        filename: "packages/bloom/src/styles/baire-category.tsx",
        lines: [57, 67, 91],
        issue:
          "Checks ball radii/nesting but draws literal 118,94,38,15 radii; dense-intersection point roles selected by x,t,z,z' labels.",
      },
      {
        ids: ["8.1"],
        filename: "packages/bloom/src/styles/bounded-sets.tsx",
        lines: [34, 46],
        issue:
          "Accepts positive bounding radius but its value does not determine the fixed box/blob coordinates.",
      },
      {
        ids: ["9.7"],
        filename: "packages/bloom/src/styles/separated-disks.tsx",
        lines: [27, 61],
        issue:
          "Rejects all disk pairs except unit disks at (0,0),(2,0); fixed source composition.",
      },
      {
        ids: ["11.20"],
        filename: "packages/bloom/src/styles/fundamental-groups.tsx",
        lines: [155, 175, 203],
        issue:
          "Two unit-winding factor facts validate the scene; literal ellipses/Beziers supply generator geometry.",
      },
      {
        ids: ["11.24"],
        filename: "packages/bloom/src/styles/planar-graph-spaces.tsx",
        lines: [93, 277],
        issue:
          "Requires eleven spaces/nine distinct glyphs and authored contour catalog rather than drawing from graph/group facts.",
      },
      {
        ids: ["11.9"],
        filename: "packages/bloom/src/styles/loops.tsx",
        lines: [111, 130],
        issue:
          "Loop endpoint/basepoint facts do not generate the fixed self-intersecting path.",
      },
      {
        ids: ["5.4", "9.8"],
        filenames: [
          "packages/bloom/src/styles/separation-axioms.tsx",
          "packages/bloom/src/styles/connected-closures.tsx",
        ],
        lines: [252, 44],
        issue:
          "Source-tuned nonlinear/piecewise display charts preserve schematic composition rather than metric coordinates.",
      },
    ],
  };
}

/** Make concurrent changes and the narrower baseline assertion explicit. */
async function recordBaselineSourceChecks(result) {
  const source = JSON.parse(
    await readFile(
      path.join(root, "docs/elementary-topology/figures.json"),
      "utf8",
    ),
  );
  const originals = JSON.parse(
    await readFile(
      path.join(root, "docs/elementary-topology/original-illustrations.json"),
      "utf8",
    ),
  );
  const records = [...source.figures, ...originals.illustrations];
  const ids = new Set(result.runs.map((run) => run.id));
  const selected = records.filter((figure) => ids.has(figure.id));
  for (const run of result.runs)
    run.source.factoryModule = selected.find(
      (figure) => figure.id === run.id,
    ).implementation;
  const files = [
    ...new Set(
      selected.flatMap((figure) => [
        figure.implementation,
        figure.style,
        figure.substanceModule ?? figure.implementation,
        figure.domainModule,
      ]),
    ),
  ].sort();
  const comparisons = await Promise.all(
    files.map(async (filename) => {
      const actual = await readFile(path.join(root, filename));
      const baseline = execFileSync(
        "git",
        ["show", `${result.baseline}:${filename}`],
        { cwd: root },
      );
      return {
        filename,
        matchesBaseline: actual.equals(baseline),
        sha256: createHash("sha256").update(actual).digest("hex"),
      };
    }),
  );
  result.baselineSourceChecks = {
    checkedAt: new Date().toISOString(),
    scope:
      "Every selected direct factory, registered style, Substance module, and domain module. Transitive helper/core sources are not claimed to be a clean baseline checkout.",
    allMatch: comparisons.every((comparison) => comparison.matchesBaseline),
    files: comparisons,
  };
  result.runtimeSnapshot = {
    modulesPreloadedBeforeRuns: true,
    primaryFingerprintsVerifiedStableDuringPreload: true,
    caveat:
      "Concurrent uncommitted core/Bloom diagnostics/geometry fixes existed as listed in sourceTrackedChangesAtStart. Results use the already loaded, cached compiled modules identified by buildFingerprints. Registered corpus sources match the stated baseline; this is not a claim that the entire worktree or every transitive source file was clean.",
    reproducibility:
      "For an exact baseline rerun, copy this audit script into a checkout of baf76d69f, build core/Bloom/examples, and run it. After fixes or new builds, the same script audits the actual emitted graph and records fresh fingerprints; those are new measurements rather than replacement evidence for this baseline snapshot.",
  };
}
