import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try/",
  plugins: [react()],

  define: {
    // By default, Vite doesn't include shims for NodeJS.
    // This is only required by  svg-flatten.
    global: {},
  },
});
