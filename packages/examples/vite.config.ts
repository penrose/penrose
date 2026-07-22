import path from "path";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

// Resolve @penrose/bloom imports to the local worktree source so that
// the jsx-runtime and other new exports are available at test runtime.
// (In the published monorepo, node_modules/@penrose/bloom already has these.)
const bloomSrc = path.resolve(__dirname, "../bloom/src");

export default defineConfig({
  build: { target: "esnext" },
  plugins: [topLevelAwait()],
  optimizeDeps: {
    esbuildOptions: { target: "esnext" },
    exclude: ["rose"],
  },
  resolve: {
    alias: {
      "@penrose/bloom/jsx-dev-runtime": path.join(bloomSrc, "jsx-runtime.ts"),
      "@penrose/bloom/jsx-runtime": path.join(bloomSrc, "jsx-runtime.ts"),
      "@penrose/bloom": path.join(bloomSrc, "index.ts"),
    },
    conditions: ["browser"],
  },
  test: { setupFiles: ["./setupVitest.js"] },
});
