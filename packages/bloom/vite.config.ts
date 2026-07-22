import path from "path";
import { defineConfig } from "vite";

export default defineConfig({
  resolve: {
    alias: {
      // Resolve @penrose/bloom/jsx-dev-runtime to local source so tests work
      // without needing the published package.
      "@penrose/bloom/jsx-dev-runtime": path.resolve(
        __dirname,
        "src/jsx-runtime.ts",
      ),
      "@penrose/bloom/jsx-runtime": path.resolve(
        __dirname,
        "src/jsx-runtime.ts",
      ),
      "@penrose/bloom": path.resolve(__dirname, "src/index.ts"),
    },
  },
  test: {
    environment: "jsdom",
  },
});
