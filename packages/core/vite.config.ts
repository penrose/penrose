import path from "path";
import { defineConfig } from "vite";

export default defineConfig({
  build: {
    outDir: "dist/bundle",
    lib: {
      entry: path.resolve(__dirname, "src/index.ts"),
      formats: ["es"],
      fileName: `index`,
    },
  },
});
