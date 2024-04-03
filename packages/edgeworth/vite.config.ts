/// <reference types="vitest" />

import react from "@vitejs/plugin-react-swc";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig({
  base: "./",
  plugins: [react(), topLevelAwait()],
  build: { target: "esnext" },
  server: {
    port: 3001,
  },
  optimizeDeps: {
    esbuildOptions: { target: "esnext" },
    exclude: ["@penrose/examples", "rose", "@penrose/components"],
  },
});
