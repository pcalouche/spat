define([
    "angular",
    "./team-manager.component",
    "./team-manager-modal.controller"
], function(angular, teamManagerComponent, TeamManagerModalController) {
    "use strict";

    return angular.module("app.team-manager", [])
        .component("teamManager", teamManagerComponent)
        .controller("TeamManagerModalController", TeamManagerModalController);
});