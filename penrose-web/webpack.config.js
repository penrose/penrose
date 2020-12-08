const path = require("path");

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
  module: {
    rules: [
      {
        test: /\.(ts|tsx)$/,
        loader: "ts-loader",
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
