import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// Fun fact: define: {global: {}} overwrites every mention of global, which breaks lots.
// Don't use with Vite. Use window.global in index.html

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
});
