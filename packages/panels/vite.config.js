// https://medium.com/@ftaioli/using-node-js-builtin-modules-with-vite-6194737c2cd2
import { NodeGlobalsPolyfillPlugin } from "@esbuild-plugins/node-globals-polyfill";
import { NodeModulesPolyfillPlugin } from "@esbuild-plugins/node-modules-polyfill";
import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try",
  plugins: [react()],
  resolve: {
    alias: {
      stream: "rollup-plugin-node-polyfills/polyfills/stream",
    },
  },
  optimizeDeps: {
    esbuildOptions: {
      // Enable esbuild polyfill plugins
      plugins: [
        NodeModulesPolyfillPlugin(),
        NodeGlobalsPolyfillPlugin({
          process: true,
          buffer: true,
        }),
      ],
    },
  },
});
