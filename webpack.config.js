const webpack = require("webpack");
const path = require("path");
const resourcesPath = path.join(__dirname, "/src/main/resources/static");
const publicPath = "dist/";
const CleanWebpackPlugin = require("clean-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const ExtractTextPlugin = require("extract-text-webpack-plugin");

module.exports = function(env) {
    console.log("resourcesPath", resourcesPath);
    if (!env) {
        console.warn("env not defined so setting to dev");
        env = "dev";
    }
    console.log("env that will be used is->", env);

    return {
        entry: {
            vendor: path.join(resourcesPath, "/src/app/vendor.js"),
            app: path.join(resourcesPath, "/src/app/app.js")
        },
        output: {
            path: path.join(resourcesPath, "/dist"),
            filename: env === "dev" ? "[name].bundle.js" : "[name].[hash].bundle.js",
            publicPath: publicPath
        },
        module: {
            rules: [{
                test: /\.js$/,
                enforce: "pre",
                include: [resourcesPath],
                use: [{
                    loader: "jshint-loader"
                }]
            }, {
                test: /\.js$/,
                include: [resourcesPath],
                use: [{
                    loader: "babel-loader",
                    options: {
                        presets: ["es2015"]
                    }
                }]
            }, {
                test: /\.html$/,
                include: [resourcesPath],
                use: [{
                    loader: "html-loader",
                    options: {
                        minimize: true
                    }
                }]
            }, {
                test: /\.css$/,
                include: [
                    path.resolve(__dirname, "node_modules/bootstrap/dist/css/"),
                    path.resolve(__dirname, "node_modules/font-awesome/css/")
                ],
                use: ExtractTextPlugin.extract({
                    use: "css-loader"
                })
            }, {
                test: /node_modules\S+\.(svg|woff(2)?|ttf|eot)$/,
                use: [{
                    loader: "file-loader",
                    options: {
                        name: env === "dev" ? "[name].[ext]" : "[name].[hash].[ext]",
                        outputPath: "assets/vendor/",
                        publicPath: "./"
                    }
                }]
            }, {
                test: /\.(png|jp(e)?g|gif|ico)$/,
                include: [resourcesPath],
                use: [{
                    loader: "file-loader",
                    options: {
                        name: env === "dev" ? "[name].[ext]" : "[name].[hash].[ext]",
                        outputPath: "assets/app/",
                        publicPath: publicPath
                    }
                }]
            }, {
                test: /\.less$/,
                include: [resourcesPath],
                use: ExtractTextPlugin.extract({
                    use: [{loader: "css-loader"},
                        {loader: "less-loader"}]
                })
            }]
        },
        resolve: {
            extensions: [".js"]
        },
        devtool: env === "dev" ? "eval-source-map" : "source-map",
        plugins: [
            new CleanWebpackPlugin([path.join(resourcesPath, "/dist")], {}),
            new HtmlWebpackPlugin({
                template: path.join(resourcesPath, "/src/app/index.html"),
                filename: path.join(resourcesPath, "/dist/index.html")
            }),
            new ExtractTextPlugin(env === "dev" ? "styles.css" : "styles.[hash].css"),
            new webpack.optimize.CommonsChunkPlugin({name: ["app", "vendor"]}),
            new webpack.LoaderOptionsPlugin({
                options: {
                    htmlLoader: {
                        // TODO try some HTML loader options to set the root
                        // root:"td-web-app"
                    },
                    jshint: {
                        camelcase: true,
                        emitErrors: false,
                        failOnHint: false
                    }
                }
            })
        ]
    }
};