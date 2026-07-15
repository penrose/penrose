#!/usr/bin/env node
// Memory-leak benchmark for the Penrose editor.
//
// Drives the real editor headlessly (Chrome via CDP), loads each example by its
// registry id (?examples=<id>), repeatedly resamples/recompiles it, and after a
// forced GC measures the main-thread V8 heap each round. A linear-regression
// slope over the rounds is the per-action leak rate — noise-robust because a true
// leak grows monotonically while GC-able churn just adds scatter around a flat line.
//
// Usage:
//   node leak-bench.mjs [options]
//
// Options:
//   --url <base>        Editor base URL (default: http://localhost:4173/try/)
//   --chrome <path>     Chrome executable (default: /usr/sbin/google-chrome-stable)
//   --action <a>        Resample | compile (default: resample)
//   --examples <ids>    Comma-separated registry ids (default: a curated set)
//   --iterations <n>    Measured rounds per example (default: 30)
//   --warmup <n>        Unmeasured warmup rounds (default: 3)
//   --settle <ms>       Max wait for the diagram to settle after each action
//                       (it usually converges sooner) (default: 8000)
//   --headful           Show the browser (debugging)
//   --interactive       Switch the diagram to EditMode and sweep the mouse over
//                       the interactive shapes each round. Only examples with
//                       translatable/scalable shapes (e.g. mobius, tree-euler)
//                       show mouse Event Listener leaks.
//   --json <path>       Write raw results as JSON
//
// Requires: puppeteer-core (installed locally in this folder) and a running editor
// server. See README.md.

import { spawnSync } from "node:child_process";
import { writeFileSync } from "node:fs";
import puppeteer from "puppeteer-core";

// Parse arguments
const argv = process.argv.slice(2);
const opt = (name, def) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && i + 1 < argv.length ? argv[i + 1] : def;
};
const flag = (name) => argv.includes(`--${name}`);

const BASE_URL = opt("url", "http://localhost:4173/try/");
const CHROME = opt("chrome", "/usr/sbin/google-chrome-stable");
const ACTION = opt("action", "resample"); // resample | compile
const ITERATIONS = parseInt(opt("iterations", "30"), 10);
const WARMUP = parseInt(opt("warmup", "3"), 10);
const SETTLE = parseInt(opt("settle", "8000"), 10);
const HEADFUL = flag("headful");
const JSON_OUT = opt("json", null);
const INTERACTIVE = flag("interactive");
const DIAGRAM_SVG = 'svg[width="100%"][height="100%"]';

const DEFAULT_EXAMPLES = [
  "array-models/insertionSort",
  "dinoshade/dinoshade",
  "set-theory-domain/tree-euler",
  "structural-formula/molecules/caffeine",
  "group-theory/quaternion-cayley-graph",
];
const EXAMPLES = opt("examples", DEFAULT_EXAMPLES.join(","))
  .split(",")
  .map((s) => s.trim())
  .filter(Boolean);

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Click a header button by its visible text ("resample" / "compile"). Returns
// false if the button is missing or currently disabled.
async function clickButton(page, label) {
  return page.evaluate((text) => {
    const btn = [...document.querySelectorAll("button")].find(
      (b) => b.textContent?.trim().toLowerCase() === text,
    );
    if (!btn || btn.disabled) return false;
    btn.click();
    return true;
  }, label);
}

// Ready = the resample button is enabled (compiling === false) and the diagram
// SVG has actually rendered shapes.
async function waitUntilReady(page, timeoutMs = 60000) {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const ready = await page.evaluate((sel) => {
      const btn = [...document.querySelectorAll("button")].find(
        (b) => b.textContent?.trim().toLowerCase() === "resample",
      );
      const enabled = btn && !btn.disabled;
      const svg = document.querySelector(sel);
      const rendered = svg && svg.querySelectorAll("*").length > 3;
      return Boolean(enabled && rendered);
    }, DIAGRAM_SVG);
    if (ready) return true;
    await sleep(250);
  }
  return false;
}

// Wait until the diagram has stopped re-rendering. The render effect replaces the
// diagram <svg> node on every streamed optimization frame; once layout converges
// the node stops being replaced. We tag the current node and consider it settled
// when the same tagged node survives two consecutive polls. Falls back to maxMs.
async function waitForSettled(page, maxMs) {
  const start = Date.now();
  let sameCount = 0;
  while (Date.now() - start < maxMs) {
    await sleep(200);
    const st = await page.evaluate((sel) => {
      const d = document.querySelector(sel);
      if (!d) return "none";
      if (d.dataset.benchSeen) return "same";
      d.dataset.benchSeen = "1";
      return "new";
    }, DIAGRAM_SVG);
    if (st === "same") {
      if (++sameCount >= 2) return true;
    } else {
      sameCount = 0;
    }
  }
  return false;
}

// Persist interactive:"EditMode" into the app's settings store (localforage /
// IndexedDB, store "keyvaluepairs") so a reload comes up in EditMode and mounts
// InteractivityOverlay. The store already exists because settingsEffect calls
// localforage.getItem("settings") on first load.
async function seedEditMode(page) {
  return page.evaluate(
    () =>
      new Promise((resolve) => {
        const open = indexedDB.open("localforage");
        open.onsuccess = () => {
          const db = open.result;
          const store = db.objectStoreNames[0];
          if (!store) return resolve("no-store");
          const tx = db.transaction(store, "readwrite");
          tx.objectStore(store).put(
            { githubAccessToken: null, vimMode: false, interactive: "EditMode" },
            "settings",
          );
          tx.oncomplete = () => resolve("ok");
          tx.onerror = () => resolve("err");
        };
        open.onerror = () => resolve("open-err");
      }),
  );
}

// Sweep the mouse across the interactive shapes to trigger mousedown and mouseover events.
async function hoverSweep(page) {
  const boxes = await page.evaluate((sel) => {
    const svg = document.querySelector(sel);
    if (!svg) return [];
    const els = [...svg.querySelectorAll('[pointer-events="visiblePainted"]')];
    return els
      .map((e) => {
        const r = e.getBoundingClientRect();
        return { x: r.x + r.width / 2, y: r.y + r.height / 2 };
      })
      .filter((b) => b.x > 0 && b.y > 0 && b.x < 1280 && b.y < 900)
      .slice(0, 20);
  }, DIAGRAM_SVG);
  for (const b of boxes) {
    await page.mouse.move(b.x, b.y);
    await page.mouse.move(2, 2); // background: hoveredPath -> null
  }
  return boxes.length;
}

// Force GC, read post-GC live memory, then measure three different stats:
//   JSHeapUsedSize   — V8 JS objects
//   DOM Nodes        — Blink DOM node count (including detached nodes
//                      still referenced by JS). This is the metric that
//                      catches the "Detached SVGSVGElement ×N" leak that
//                      was documented.
//   JSEventListeners — leaked addEventListener registrations
async function measure(client) {
  await client.send("HeapProfiler.collectGarbage");
  const { metrics } = await client.send("Performance.getMetrics");
  const m = Object.fromEntries(metrics.map((x) => [x.name, x.value]));
  return {
    heap: m.JSHeapUsedSize ?? 0,
    nodes: m.Nodes ?? 0,
    listeners: m.JSEventListeners ?? 0,
  };
}

// Ordinary-least-squares slope + R^2 of y over x = [0..n-1].
function regression(ys) {
  const n = ys.length;
  const xs = ys.map((_, i) => i);
  const mx = xs.reduce((a, b) => a + b, 0) / n;
  const my = ys.reduce((a, b) => a + b, 0) / n;
  let sxy = 0,
    sxx = 0,
    syy = 0;
  for (let i = 0; i < n; i++) {
    sxy += (xs[i] - mx) * (ys[i] - my);
    sxx += (xs[i] - mx) ** 2;
    syy += (ys[i] - my) ** 2;
  }
  const slope = sxx === 0 ? 0 : sxy / sxx;
  const r2 = sxx === 0 || syy === 0 ? 0 : (sxy * sxy) / (sxx * syy);
  return { slope, r2 };
}

const kb = (bytes) => (bytes / 1024).toFixed(1);

// Action that reloads the diagram
async function doAction(page) {
  if (ACTION === "compile") {
    await clickButton(page, "compile");
    await waitForSettled(page, SETTLE);
  } else if (ACTION === "resample") {
    await clickButton(page, "resample");
    await waitForSettled(page, SETTLE);
  }
}

async function benchExample(browser, id) {
  const page = await browser.newPage();
  const client = await page.target().createCDPSession();
  await client.send("HeapProfiler.enable");
  await client.send("Performance.enable");

  const url = `${BASE_URL}?examples=${encodeURIComponent(id)}`;
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 60000 });

  let ready = await waitUntilReady(page);
  if (!ready) {
    await page.close();
    return { id, error: "did not become ready (compile failed or timed out)" };
  }

  let interactiveShapes = null;
  if (INTERACTIVE) {
    // Turn on EditMode, then reload so the app boots into it and mounts the
    // overlay. The ?examples query survives the reload and recompiles.
    await seedEditMode(page);
    await page.reload({ waitUntil: "domcontentloaded", timeout: 60000 });
    ready = await waitUntilReady(page);
    if (!ready) {
      await page.close();
      return { id, error: "did not become ready after EditMode reload" };
    }
    await waitForSettled(page, SETTLE); // let the overlay mount + settle
    interactiveShapes = await hoverSweep(page);
  }

  // Warmup: let JIT / caches / first-run allocations settle so they don't
  // masquerade as a leak in the first measured rounds.
  for (let i = 0; i < WARMUP; i++) {
    await doAction(page);
    if (INTERACTIVE) await hoverSweep(page);
  }

  const heaps = [];
  const nodes = [];
  const listeners = [];
  for (let i = 0; i < ITERATIONS; i++) {
    await doAction(page);
    if (INTERACTIVE) await hoverSweep(page);
    const s = await measure(client);
    heaps.push(s.heap);
    nodes.push(s.nodes);
    listeners.push(s.listeners);
    process.stdout.write(
      `\r  ${id}: round ${i + 1}/${ITERATIONS}  ` +
        `heap=${kb(s.heap)}kB nodes=${s.nodes} listeners=${s.listeners}   `,
    );
  }
  process.stdout.write("\n");

  await page.close();

  const heapReg = regression(heaps);
  const nodeReg = regression(nodes);
  const listenerReg = regression(listeners);
  return {
    id,
    rounds: ITERATIONS,
    interactive: INTERACTIVE,
    interactiveShapes,
    listenerR2: listenerReg.r2,
    heapFirst: heaps[0],
    heapLast: heaps[heaps.length - 1],
    heapSlopeBytes: heapReg.slope,
    heapR2: heapReg.r2,
    nodeFirst: nodes[0],
    nodeLast: nodes[nodes.length - 1],
    nodeSlope: nodeReg.slope,
    nodeR2: nodeReg.r2,
    listenerFirst: listeners[0],
    listenerLast: listeners[listeners.length - 1],
    listenerSlope: listenerReg.slope,
    heaps,
    nodes,
    listeners,
  };
}

async function main() {
  // Check if a server is listening
  const probe = spawnSync("curl", ["-fsS", "-o", "/dev/null", BASE_URL], {
    timeout: 10000,
  });
  if (probe.status !== 0) {
    console.error(
      `\n✗ Editor not reachable at ${BASE_URL}\n` +
        `  Start it first (see README.md), e.g.:\n` +
        `    yarn workspace @penrose/editor build && \\\n` +
        `    npx vite preview --config packages/editor/vite.config.ts --port 4173\n`,
    );
    process.exit(1);
  }

  // Show benchmark configuration
  console.log(`\nPenrose editor memory-leak benchmark`);
  console.log(`  url        ${BASE_URL}`);
  console.log(`  action     ${ACTION}${INTERACTIVE ? " + EditMode hover sweep" : ""}`);
  console.log(`  rounds     ${ITERATIONS} (+${WARMUP} warmup)`);
  console.log(`  settle     ${SETTLE}ms`);
  console.log(`  examples   ${EXAMPLES.length}\n`);

  // Launch puppeteer
  const browser = await puppeteer.launch({
    executablePath: CHROME,
    headless: !HEADFUL,
    args: [
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--js-flags=--expose-gc",
    ],
    defaultViewport: { width: 1280, height: 900 },
  });

  // Run benchmarks
  const results = [];
  try {
    for (const id of EXAMPLES) {
      results.push(await benchExample(browser, id));
    }
  } finally {
    await browser.close();
  }

  // Show results
  const pad = (s, n) => String(s).padEnd(n);
  const padL = (s, n) => String(s).padStart(n);
  console.log(`\n${"=".repeat(94)}`);
  console.log(`RESULTS  (per-round = OLS slope over ${ITERATIONS} rounds, post-GC)`);
  console.log("=".repeat(94));
  console.log(
    pad("example", 40) +
      padL("heap kB/rd", 12) +
      padL("nodes/rd", 10) +
      padL("listeners/rd", 14) +
      padL("Δnodes", 9),
  );
  console.log("-".repeat(94));

  // Sort by DOM-node leak then heap leak
  const ok = results.filter((r) => !r.error);
  ok.sort((a, b) => b.nodeSlope - a.nodeSlope || b.heapSlopeBytes - a.heapSlopeBytes);
  for (const r of ok) {
    const dNodes = r.nodeLast - r.nodeFirst;
    // Flag a likely real leak if there is steady positive growth in either the
    // DOM-node count or the JS heap.
    const leaky =
      (r.nodeSlope > 5 && r.nodeR2 > 0.8) ||
      (r.heapSlopeBytes / 1024 > 20 && r.heapR2 > 0.8);
    console.log(
      pad(r.id, 40) +
        padL(kb(r.heapSlopeBytes), 12) +
        padL(r.nodeSlope.toFixed(1), 10) +
        padL(r.listenerSlope.toFixed(1), 14) +
        padL(dNodes, 9) +
        (leaky ? "  ⚠ leak" : ""),
    );
  }

  // Show which examples errored out, if any
  for (const r of results.filter((r) => r.error)) {
    console.log(pad(r.id, 40) + "  ✗ " + r.error);
  }

  console.log("=".repeat(94));
  console.log("⚠ leak = steady growth (R²>0.8) in nodes (>5/rd) or heap (>20kB/rd)\n");

  // Write to JSON file
  if (JSON_OUT) {
    writeFileSync(JSON_OUT, JSON.stringify(results, null, 2));
    console.log(`raw results → ${JSON_OUT}\n`);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
