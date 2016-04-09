define([
    // Required Angular Modules
    "angular",
    "ngAnimate",
    "ngResource",
    // Required AngularUI Modules
    "uiBootstrap",
    "uiRouter",
    // Other Angular Modules
    "smartTable",
    // Shared Application Modules
    "components/general-components/general-components.module",
    "components/services/services.module"
], function(angular, ngAnimate, ngResource, uiBootstrap, uiRouter, smartTable, generalComponentsModule, servicesModule) {
    "use strict";

    return angular.module("app.core", [
        // Required Angular Modules
        "ngAnimate",
        "ngResource",
        // Required AngularUI Modules
        "ui.bootstrap",
        "ui.router",
        // Other Angular Modules
        "smart-table",
        // Shared Application Modules
        generalComponentsModule.name,
        servicesModule.name
    ]);
});