import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vite.dev/config/
export default defineConfig({
  plugins: [react(), topLevelAwait()],
  optimizeDeps: {
    exclude: ["rose"],
  },
});
