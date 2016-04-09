define([
    "angular",
    "module",
    "./core.module",
    "components/main-navigation/main-navigation.module",
    "components/features/features.module"
], function(angular, module) {
    "use strict";

    routeConfig.$inject = ["$httpProvider", "$locationProvider", "$stateProvider", "$urlRouterProvider"];

    function routeConfig($httpProvider, $locationProvider, $stateProvider, $urlRouterProvider) {
        $httpProvider.defaults.headers.common["AUTH_TOKEN"] = "myAuthCode"
        $locationProvider.html5Mode(true);
        $urlRouterProvider.otherwise("team-manager");

        $stateProvider.state("team-manager", {
            url: "/team-manager",
            templateUrl: "resources/app/components/features/team-manager/team-manager.html",
            controller: "TeamManagerController",
            controllerAs: "vm"
        }).state("user-manager", {
            url: "/user-manager",
            templateUrl: "resources/app/components/features/user-manager/user-manager.html",
            controller: "UserManagerController",
            controllerAs: "vm"
        });
    }

    //The Main App Module
    return angular.module("app", [
        // Required App Modules
        "app.core",
        "app.main-navigation",
        "app.features"
    ]).config(routeConfig).constant("spatGlobals", module.config().spatGlobals);
});
