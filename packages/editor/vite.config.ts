import react from "@vitejs/plugin-react-swc";
import * as path from "path";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try/",
  plugins: [react({ jsxRuntime: "classic" })],
  build: { target: "esnext" },
  optimizeDeps: {
    exclude: ["@penrose/examples"],
  },
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
