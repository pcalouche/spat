define([
    "angular",
    "./main-app.component"
], function(angular, mainAppComponent) {
    "use strict";

    return angular.module("app.main-app", [])
        .component("mainApp", mainAppComponent);
});