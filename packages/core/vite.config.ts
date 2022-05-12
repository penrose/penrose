import { NodeModulesPolyfillPlugin } from "@esbuild-plugins/node-modules-polyfill";
import path from "path";
import { visualizer } from "rollup-plugin-visualizer";
import { defineConfig } from "vite";
import tsconfigPaths from "vite-tsconfig-paths";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [tsconfigPaths()],
  build: {
    sourcemap: true,
    lib: {
      name: "@penrose/core",
      formats: ["umd", "cjs", "es"],
      entry: path.resolve(__dirname, "src/index.ts"),
      fileName: (format) => `index.${format}.js`,
    },
    rollupOptions: {
      plugins: [visualizer()],
    },
  },
  optimizeDeps: {
    esbuildOptions: {
      // Enable esbuild polyfill plugins
      plugins: [NodeModulesPolyfillPlugin()],
    },
  },
});
