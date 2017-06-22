import template from "./team-manager-modal.component.html";

TeamManagerModalController.$inject = [];

function TeamManagerModalController() {
    let ctrl = this;

    ctrl.$onInit = function() {
        let modalData = ctrl.resolve.modalData;
        ctrl.title = modalData.action === "Add" ? "Add Team" : "Edit Team";
        ctrl.team = modalData.team;
    };

    ctrl.save = function() {
        ctrl.modalInstance.close(ctrl.team);
    };

    ctrl.cancel = function() {
        ctrl.modalInstance.dismiss("cancel");
    };
}

export const teamManagerModalComponent = {
    template: template,
    controller: TeamManagerModalController,
    bindings: {
        modalInstance: "<",
        resolve: "<"
    }
};