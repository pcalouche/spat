import angular from "angular";
modalService.$inject = ["$uibModal"];

export function modalService($uibModal) {
    let modalDefaultConfig = {
        animation: true,
        component: "basicModal",
        size: "md"
    };

    this.showModal = function(customModalConfig) {
        // Merge modal custom config into the default config
        let modalConfigToUse = angular.merge({}, modalDefaultConfig, customModalConfig);
        return $uibModal.open(modalConfigToUse).result;
    };
}