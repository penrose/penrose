import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try/",
  worker: {
    format: "es",
  },
  plugins: [react({ jsxRuntime: "classic" })],
  build: { target: "esnext" },
});
