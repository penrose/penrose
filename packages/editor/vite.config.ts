import react from "@vitejs/plugin-react-swc";
import * as path from "path";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

const isProduction = process.env.NODE_ENV === "production";

const profiling = isProduction && {
  "react-dom/client": "react-dom/profiling",
};

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try/",
  plugins: [react({ jsxRuntime: "classic" }), topLevelAwait()],
  worker: {
    format: "es",
    plugins: () => [topLevelAwait()],
  },
  build: {
    target: "esnext",
    minify: false,
  },
  optimizeDeps: {
    esbuildOptions: { target: "esnext", minify: false },
    exclude: ["@penrose/examples", "rose"],
  },
  server: {
    port: 3000,
    headers: {
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Opener-Policy": "same-origin",
    },
  },
  preview: {
    headers: {
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Opener-Policy": "same-origin",
    },
  },
  resolve: {
    preserveSymlinks: true,
    alias: {
      ...profiling,
      "@penrose/components": `${path.resolve("..")}/components/src`,
      "@penrose/core": `${path.resolve("..")}/core/src`,
    },
  },
});
