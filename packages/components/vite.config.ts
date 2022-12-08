import react from "@vitejs/plugin-react";
import path from "path";
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), topLevelAwait()],
  build: {
    sourcemap: true,
    lib: {
      name: "@penrose/components",
      formats: ["es"],
      entry: path.resolve(__dirname, "src/index.ts"),
      fileName: (format) => `index.${format}.js`,
    },
    rollupOptions: {
      external: ["react", "react-dom"],
      output: {
        globals: {
          react: "React",
        },
      },
    },
  },
});
