define([
    "angular",
    "./basic-modal.controller"
], function(angular, BasicModalController) {
    "use strict";

    return angular.module("general-components.basic-modal", [])
        .controller("BasicModalController", BasicModalController);
});
