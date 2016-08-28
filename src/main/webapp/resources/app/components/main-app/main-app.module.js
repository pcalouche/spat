define([
    "angular",
    "./main-app.component",
    "./main-app.service"
], function(angular, mainAppComponent, mainAppService) {
    "use strict";

    return angular.module("app.main-app", [])
        .component("mainApp", mainAppComponent)
        .service("mainAppService", mainAppService);
});