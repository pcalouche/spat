define([
    "angular"
], function(angular) {
    "use strict";

    modalService.$inject = ["$uibModal"];

    function modalService($uibModal) {

        var modalDefaultConfig = {
            animation: true,
            component: "basicModal",
            controllerAs: "vm",
            size: "md"
        };

        this.showModal = function(customModalConfig) {
            // Merge modal custom config into the default config
            var modalConfigToUse = angular.merge({}, modalDefaultConfig, customModalConfig);
            return $uibModal.open(modalConfigToUse).result;
        };
    }

    return modalService;
});
