define([
    "angular",
    "module", // This a handle to the RequireJS's special module dependency in order to get config info
    "./core.module",
    "components/main-app/main-app.module",
    "components/features/features.module"
], function(angular, module, coreModule, mainAppModule, featuresModule) {
    "use strict";

    interceptors.$inject = ["spatGlobals"];

    function interceptors(spatGlobals) {
        var versionToUse = spatGlobals.version;
        // If environment is DEV then use the current timestamp as the version to avoid caching of HTML during development
        if (spatGlobals.environment == "dev") {
            versionToUse = new Date().getTime();
        }
        // Returns object representing AngularJS http interceptors for app
        return {
            request: function(config) {
                config.headers ["AUTH_TOKEN"] = "myAuthCode";
                config.headers["Cache-Control"] = "no-cache";
                config.headers["Pragma"] = "no-cache";
                var urReqExp = /components[(\w|\/)|\-|\.]*.html/;
                if (config.method == "GET" && urReqExp.test(config.url)) {
                    config.params = config.params ? config.params : {};
                    config.params.version = versionToUse;
                }
                return config;
            }
        };
    }

    appConfig.$inject = ["$httpProvider", "$locationProvider", "$urlRouterProvider"];

    function appConfig($httpProvider, $locationProvider, $urlRouterProvider) {
        $httpProvider.interceptors.push(interceptors);
        $locationProvider.html5Mode(true);
        $urlRouterProvider.otherwise("team-manager");

        // In a large project the routes can get quite long.  To avoid a large list, each feature module
        // declares its own config for routes and anything else that may be needed
    }

    // The Main App Module
    return angular.module("app", [
        // Required App Modules
        coreModule.name,
        mainAppModule.name,
        featuresModule.name
    ]).constant("spatGlobals", module.config().spatGlobals)
        .config(appConfig);
});
