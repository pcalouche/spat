var gulp = require("gulp");
var path = require("path");
var exec = require("child_process").exec;

gulp.task("default", function() {
    // place code for your default task here
});

gulp.task("optimizeRequireJS", function(cb) {
    var appPath = path.join(__dirname, "/src/main/webapp/resources/app");
    var rjsPath = path.join(appPath, "/lib/require/2.3.2/r.js");
    var rjsBuildConfigPath = path.join(appPath, "/build.config.js");
    var optimizeRequireJsTask = "node \"" + rjsPath + "\" -o \"" + rjsBuildConfigPath + "\"";

    exec(optimizeRequireJsTask, function(err, stdout, stderr) {
        console.error(stdout);
        console.error(stderr);
        cb(err);
    });
});