import react from "@vitejs/plugin-react-swc";
import * as path from "path";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try/",
  plugins: [react({ jsxRuntime: "classic" }), topLevelAwait()],
  build: { target: "esnext" },
  server: {
    port: 3000,
  },
  resolve: {
    preserveSymlinks: true,
    alias: {
      "@penrose/components": `${path.resolve("..")}/components/src`,
      "@penrose/core": `${path.resolve("..")}/core/src`,
    },
  },
});
