define([
    "angular",
    "./team-manager.component",
    "./team-manager-modal.component"
], function(angular, teamManagerComponent, TeamManagerModalComponent) {
    "use strict";

    moduleConfig.$inject = ["$stateProvider"];

    function moduleConfig($stateProvider) {
        $stateProvider.state("team-manager", {
            url: "/team-manager",
            template: "<team-manager></team-manager>"
        });
    }

    return angular.module("app.team-manager", [])
        .config(moduleConfig)
        .component("teamManager", teamManagerComponent)
        .component("teamManagerModal", TeamManagerModalComponent);
});