module.exports = {
  stories: ["../src/**/*.stories.mdx", "../src/**/*.stories.@(js|jsx|ts|tsx)"],
  addons: [
    "@storybook/addon-links",
    "@storybook/addon-essentials",
    "@storybook/addon-interactions",
  ],
  framework: "@storybook/react",
  core: {
    builder: "@storybook/builder-vite",
  },
  features: {
    storyStoreV7: true,
  },
  viteFinal: async (config, { configType }) => {
    if (configType === "PRODUCTION") {
      config.base = "./";
    }
    config = {
      ...(config ?? {}),
      worker: {
        format: "es",
      },
      build: {
        target: "esnext",
      },
    };
    return config;
  },
};
