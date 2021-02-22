const path = require("path");
const BundleAnalyzerPlugin = require("webpack-bundle-analyzer")
  .BundleAnalyzerPlugin;

module.exports = {
  entry: "./src/module.ts",
  resolve: {
    extensions: [".js", ".jsx", ".json", ".ts", ".tsx"],
    modules: [path.resolve(__dirname, "src"), "node_modules"],
  },
  // https://github.com/webpack-contrib/css-loader/issues/447
  node: {
    fs: "empty",
  },
  // tree shaking
  // mode: "production",
  mode: "development",

  plugins: [new BundleAnalyzerPlugin()],

  module: {
    rules: [
      {
        test: /\.(ts|tsx)$/,
        use: [
          {
            loader: "ts-loader",
            options: {
              configFile: "tsconfig.lib.json",
            },
          },
        ],
      },
    ],
  },
  output: {
    path: path.resolve(__dirname, "build/dist"),
    filename: "penrose.js",
    library: "penrose",
  },
  externals: {},
};
