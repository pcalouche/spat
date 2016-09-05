define([
    "angular",
    "./user-manager.component",
    "./user-manager-modal.component"
], function(angular, userManagerComponent, userManagerModalComponent) {
    "use strict";

    moduleConfig.$inject = ["$stateProvider"];

    function moduleConfig($stateProvider) {
        $stateProvider.state("user-manager", {
            url: "/user-manager",
            template: "<user-manager></user-manager>"
        });
    }

    return angular.module("app.user-manager", [])
        .config(moduleConfig)
        .component("userManager", userManagerComponent)
        .component("userManagerModal", userManagerModalComponent);
});