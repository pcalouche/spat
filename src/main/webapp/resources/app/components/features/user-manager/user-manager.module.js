define([
    "angular",
    "./user-manager.component",
    "./user-manager-modal.component"
], function(angular, userManagerComponent, userManagerModalComponent) {
    "use strict";

    return angular.module("app.user-manager", [])
        .component("userManager", userManagerComponent)
        .component("userManagerModal", userManagerModalComponent);
});