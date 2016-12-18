module.exports = {
  entry: "./main.js",
  devtool: "#source-map",
  output: {
    path: __dirname,
    filename: "bundle.js"
  },
  resolve: {
    alias: {
      jquery: "jquery/src/jquery"
    }
  }
};
