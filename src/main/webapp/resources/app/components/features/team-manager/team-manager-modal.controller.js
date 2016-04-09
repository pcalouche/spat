define([], function() {
    "use strict";

    TeamManagerModalController.$inject = ["$uibModalInstance", "modalData"];

    function TeamManagerModalController($uibModalInstance, modalData) {
        var vm = this;
        vm.title = modalData.action == "Add" ? "Add Team" : "Edit Team";
        vm.team = modalData.team;

        vm.save = function() {
            $uibModalInstance.close(vm.team);
        };

        vm.cancel = function() {
            $uibModalInstance.dismiss("cancel");
        };
    }

    return TeamManagerModalController;
});