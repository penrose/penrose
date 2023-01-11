import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  worker: {
    format: "es",
  },
  build: { target: "esnext" },
});
