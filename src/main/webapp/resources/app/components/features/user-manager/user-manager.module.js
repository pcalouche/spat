define([
    "angular",
    "./user-manager.controller",
    "./user-manager-modal.controller"
], function(angular, UserManagerController, UserManagerModalController) {
    "use strict";

    return angular.module("app.user-manager", [])
        .controller("UserManagerController", UserManagerController)
        .controller("UserManagerModalController", UserManagerModalController);
});