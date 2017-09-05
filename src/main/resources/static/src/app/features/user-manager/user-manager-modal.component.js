import template from "./user-manager-modal.component.html";

UserManagerModalController.$inject = [];

function UserManagerModalController() {
    let ctrl = this;

    ctrl.$onInit = function() {
        let modalData = ctrl.resolve.modalData;
        ctrl.title = modalData.action === "Add" ? "Add User" : "Edit User";
        ctrl.user = modalData.user;
    };

    ctrl.save = function() {
        ctrl.modalInstance.close(ctrl.user);
    };

    ctrl.cancel = function() {
        ctrl.modalInstance.dismiss("cancel");
    };
}

export const userManagerModalComponent = {
    template: template,
    controller: UserManagerModalController,
    bindings: {
        modalInstance: "<",
        resolve: "<"
    }
};