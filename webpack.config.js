// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');

module.exports = {
    // mode: "development",
    mode: 'production',
    optimization: {
        minimizer: [new UglifyJsPlugin()],
    },
    // devtool: 'source-map',
    entry: "./src/App.fs.js",
    output: {
        path: path.join(__dirname, "./docs"),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./docs",
        port: 8080,
    },
    module: {
    },
    performance: {
        maxEntrypointSize: 512000,
        maxAssetSize: 512000
    }
}
