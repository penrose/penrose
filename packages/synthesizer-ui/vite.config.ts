import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "./",
  plugins: [react()],
  server: {
    port: 3001,
  },
  optimizeDeps: {
    exclude: ["@penrose/core", "@penrose/components"],
  },
});
