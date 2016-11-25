define([
    // Required Angular Modules
    "angular",
    "ngAnimate",
    "ngResource",
    // Required AngularUI Modules
    "uiBootstrap",
    "uiRouter",
    // Shared Application Modules
    "components/general-components/general-components.module",
    "components/services/services.module"
], function(angular, ngAnimate, ngResource, uiBootstrap, uiRouter, generalComponentsModule, servicesModule) {
    "use strict";

    return angular.module("app.core", [
        // Required Angular Modules
        "ngAnimate",
        "ngResource",
        // Required AngularUI Modules
        "ui.bootstrap",
        "ui.router",
        // Shared Application Modules
        generalComponentsModule.name,
        servicesModule.name
    ]);
});