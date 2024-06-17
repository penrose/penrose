import react from "@vitejs/plugin-react-swc";
import * as path from "path";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try/",
  plugins: [react({ jsxRuntime: "classic" }), topLevelAwait()],
  worker: {
    format: "es",
    plugins: [topLevelAwait()],
  },
  build: { target: "esnext" },
  optimizeDeps: {
    esbuildOptions: { target: "esnext" },
    exclude: ["@penrose/examples", "rose"],
  },
  server: {
    port: 3000,
    headers: { "Cross-Origin-Opener-Policy": "same-origin-allow-popups" },
  },
  preview: {
    headers: { "Cross-Origin-Opener-Policy": "same-origin-allow-popups" },
  },
  resolve: {
    preserveSymlinks: true,
    alias: {
      "@penrose/components": `${path.resolve("..")}/components/src`,
      "@penrose/core": `${path.resolve("..")}/core/src`,
    },
  },
});
