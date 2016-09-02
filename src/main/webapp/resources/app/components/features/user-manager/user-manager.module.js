define([
    "angular",
    "./user-manager.component",
    "./user-manager-modal.component"
], function(angular, userManagerComponent, userManagerModalComponent) {
    "use strict";

    routeConfig.$inject = ["$stateProvider"];

    function routeConfig($stateProvider) {
        $stateProvider.state("user-manager", {
            url: "/user-manager",
            template: "<user-manager></user-manager>"
        });
    }

    return angular.module("app.user-manager", [])
        .config(routeConfig)
        .component("userManager", userManagerComponent)
        .component("userManagerModal", userManagerModalComponent);
});