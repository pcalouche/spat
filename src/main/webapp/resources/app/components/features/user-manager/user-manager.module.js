define([
    "angular",
    "./user-manager.component",
    "./user-manager-modal.controller"
], function(angular, userManagerComponent, UserManagerModalController) {
    "use strict";

    return angular.module("app.user-manager", [])
        .component("userManager", userManagerComponent)
        .controller("UserManagerModalController", UserManagerModalController);
});