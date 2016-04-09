define([
    "angular"
], function(angular) {
    "use strict";

    modalService.$inject = ["$uibModal"];

    function modalService($uibModal) {

        var modalDefaultConfig = {
            animation: true,
            templateUrl: "resources/app/components/general-components/basic-modal/basic-modal.html",
            controller: "BasicModalController",
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
