define([
    "angular",
    "./team-manager.component",
    "./team-manager-modal.component"
], function(angular, teamManagerComponent, TeamManagerModalComponent) {
    "use strict";

    return angular.module("app.team-manager", [])
        .component("teamManager", teamManagerComponent)
        .component("teamManagerModal", TeamManagerModalComponent);
});