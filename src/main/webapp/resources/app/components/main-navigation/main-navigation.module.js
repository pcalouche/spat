define([
    "angular",
    "./main-navigation.controller"
], function(angular, MainNavigationController) {
    "use strict";

    return angular.module("app.main-navigation", [])
        .controller("MainNavigationController", MainNavigationController);
});