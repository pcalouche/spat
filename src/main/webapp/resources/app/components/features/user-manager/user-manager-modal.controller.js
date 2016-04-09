define([], function() {
    "use strict";

    UserManagerModalController.$inject = ["$uibModalInstance", "modalData"];

    function UserManagerModalController($uibModalInstance, modalData) {
        var vm = this;
        vm.title = modalData.action == "Add" ? "Add User" : "Edit User";
        vm.user = modalData.user;

        vm.save = function() {
            $uibModalInstance.close(vm.user);
        };

        vm.cancel = function() {
            $uibModalInstance.dismiss("cancel");
        };
    }

    return UserManagerModalController;
});