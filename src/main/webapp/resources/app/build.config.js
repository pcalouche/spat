// RequireJS build config file.  Node.js needs to be installed in order to r.js.  This config file can then be passed an option like so:
// node r.js -o ..\..\..\build.config.js
({
    name: "main",
    baseUrl: "./",
    mainConfigFile: "./main.js",
    out: "./main-optimized.min.js",
    generateSourceMaps: true,
    preserveLicenseComments: false,
    optimize: "uglify2"
})