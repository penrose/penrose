import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  rollupOptions: {
    external: ["react", "react-dom"],
    output: {
      globals: {
        react: "React",
      },
    },
  },
});
