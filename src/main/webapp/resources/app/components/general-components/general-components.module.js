define([
    "angular",
    "./basic-modal/basic-modal.component"
], function(angular, basicModalComponent) {
    "use strict";

    return angular.module("app.general-components", [])
        .component("basicModal", basicModalComponent);
});