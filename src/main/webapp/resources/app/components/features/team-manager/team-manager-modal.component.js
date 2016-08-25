define([], function() {
    "use strict";

    var teamManagerModalComponent = {
        templateUrl: "resources/app/components/features/team-manager/team-manager-modal.component.html",
        controller: TeamManagerModalController,
        bindings: {
            modalInstance: "<",
            resolve: "<"
        },
        controllerAs: "vm"
    };

    TeamManagerModalController.$inject = [];

    function TeamManagerModalController() {
        var vm = this;
        var modalData = vm.resolve.modalData;
        vm.title = modalData.action == "Add" ? "Add Team" : "Edit Team";
        vm.team = modalData.team;

        vm.save = function() {
            vm.modalInstance.close(vm.team);
        };

        vm.cancel = function() {
            vm.modalInstance.dismiss("cancel");
        };
    }

    return teamManagerModalComponent;
});
