import { defineConfig } from "vite";

// run SolidJS as if in the browser
export default defineConfig({ resolve: { conditions: ["browser"] } });
