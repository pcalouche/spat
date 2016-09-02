define([
    "angular",
    "module", // This a handle to the RequireJS's special module dependency in order to get config info
    "./core.module",
    "components/main-app/main-app.module",
    "components/features/features.module"
], function(angular, module, coreModule, mainAppModule, featuresModule) {
    "use strict";

    routeConfig.$inject = ["$httpProvider", "$locationProvider", "$urlRouterProvider"];

    function routeConfig($httpProvider, $locationProvider, $urlRouterProvider) {
        $httpProvider.defaults.headers.common["AUTH_TOKEN"] = "myAuthCode";
        $locationProvider.html5Mode(true);
        $urlRouterProvider.otherwise("team-manager");

        // In a large project the routes can get quite long.  To avoid a large list,
        // each feature module declares its own route config
    }

    // The Main App Module
    return angular.module("app", [
        // Required App Modules
        coreModule.name,
        mainAppModule.name,
        featuresModule.name
    ]).constant("spatGlobals", module.config().spatGlobals)
        .config(routeConfig);
});
