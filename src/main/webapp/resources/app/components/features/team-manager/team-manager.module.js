define([
    "angular",
    "./team-manager.controller",
    "./team-manager-modal.controller"
], function(angular, TeamManagerController, TeamManagerModalController) {
    "use strict";

    return angular.module("app.team-manager", [])
        .controller("TeamManagerController", TeamManagerController)
        .controller("TeamManagerModalController", TeamManagerModalController);
});