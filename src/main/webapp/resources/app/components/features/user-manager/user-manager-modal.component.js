define([], function() {
    "use strict";

    var userManagerModalComponent = {
        templateUrl: "resources/app/components/features/user-manager/user-manager-modal.component.html",
        controller: UserManagerModalController,
        bindings: {
            modalInstance: "<",
            resolve: "<"
        },
        controllerAs: "vm"
    };

    UserManagerModalController.$inject = [];

    function UserManagerModalController() {
        var vm = this;
        var modalData = vm.resolve.modalData;
        vm.title = modalData.action == "Add" ? "Add User" : "Edit User";
        vm.user = modalData.user;

        vm.save = function() {
            vm.modalInstance.close(vm.user);
        };

        vm.cancel = function() {
            vm.modalInstance.dismiss("cancel");
        };
    }

    return userManagerModalComponent;
});
