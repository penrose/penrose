import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "./",
  plugins: [react({ jsxRuntime: "classic" })],
  optimizeDeps: {
    exclude: ["@penrose/core", "@penrose/components"],
  },
  build: { target: "esnext" },
});
