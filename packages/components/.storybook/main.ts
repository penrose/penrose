import type { StorybookConfig } from "@storybook/react-vite";
import topLevelAwait from "vite-plugin-top-level-await";

const config: StorybookConfig = {
  framework: "@storybook/react-vite",
  stories: ["../src/**/*.mdx", "../src/**/*.stories.@(js|jsx|mjs|ts|tsx)"],
  addons: ["@storybook/addon-links", "@storybook/addon-essentials"],
  core: {
    builder: "@storybook/builder-vite", // 👈 The builder enabled here.
  },
  async viteFinal(config) {
    // framework
    // Merge custom configuration into the default config
    const { mergeConfig } = await import("vite");

    return mergeConfig(config, {
      // Add dependencies to pre-optimization
      optimizeDeps: {
        esbuildOptions: { target: "esnext" },
        exclude: ["rose"],
      },
      plugins: [topLevelAwait()],
    });
  },
};

export default config;
