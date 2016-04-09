define([
    "angular",
    "./basic-modal/basic-modal.controller"
], function(angular, BasicModalController) {
    "use strict";

    return angular.module("app.general-components", [])
        .controller("BasicModalController", BasicModalController);
});