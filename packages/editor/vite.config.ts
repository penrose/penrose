import react from "@vitejs/plugin-react-swc";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  base: "/try/",
  plugins: [react({ jsxRuntime: "classic" })],
  build: { target: "esnext" },
  server: {
    port: 3000,
  },
});
