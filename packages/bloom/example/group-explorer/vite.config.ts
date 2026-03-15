import react from "@vitejs/plugin-react";
import { fileURLToPath } from "url";
import path from "path";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
// 4 levels up from packages/bloom/example/group-explorer → worktree root
const worktreeRoot = path.resolve(__dirname, "../../../../");
// The main repo sits 3 levels above the worktree root (.claude/worktrees/<name>)
const mainRepo = path.resolve(worktreeRoot, "../../..");

export default defineConfig({
  plugins: [react(), topLevelAwait()],
  build: { target: "esnext" },
  optimizeDeps: {
    esbuildOptions: { target: "esnext" },
    // rose uses top-level await + WASM; must not be pre-bundled by Vite
    exclude: ["rose"],
  },
  server: {
    fs: {
      // Allow serving files from the main repo (for @penrose/* dist and rose wasm)
      allow: [worktreeRoot, mainRepo],
    },
  },
  resolve: {
    alias: {
      "@penrose/bloom": path.resolve(mainRepo, "packages/bloom/dist/index.js"),
      "@penrose/core": path.resolve(mainRepo, "packages/core/dist/index.js"),
    },
    // Use browser exports so @rose-lang/wasm loads its browser/WASM build
    conditions: ["browser"],
  },
});
