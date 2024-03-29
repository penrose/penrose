import { defineConfig } from "vite";
import solidPlugin from "vite-plugin-solid";
import topLevelAwait from "vite-plugin-top-level-await";

export default defineConfig({
  plugins: [solidPlugin(), topLevelAwait()],
  build: { target: "esnext" },
  optimizeDeps: {
    esbuildOptions: { target: "esnext" },
    exclude: ["rose"],
  },
});
