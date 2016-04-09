define([
    "angular",
    "./team-manager/team-manager.module",
    "./user-manager/user-manager.module"
], function(angular, teamManagerModule, userManagerModule) {
    "use strict";

    return angular.module("app.features", [teamManagerModule.name, userManagerModule.name]);
});