import template from "./basic-modal.component.html";

BasicModalController.$inject = [];

function BasicModalController() {
    let ctrl = this;

    ctrl.$onInit = function() {
        let modalData = ctrl.resolve.modalData;
        ctrl.title = modalData.title;
        ctrl.message = modalData.message;
        ctrl.includeCancelButton = modalData.includeCancelButton ? modalData.includeCancelButton : false;
        ctrl.okText = modalData.okText ? modalData.okText : "Ok";
        ctrl.cancelText = modalData.cancelText ? modalData.cancelText : "Cancel";
    };

    ctrl.ok = function() {
        ctrl.modalInstance.close();
    };

    ctrl.cancel = function() {
        ctrl.modalInstance.dismiss({$value: "cancel"});
    };
}

export const basicModalComponent = {
    template: template,
    controller: BasicModalController,
    bindings: {
        modalInstance: "<",
        resolve: "<"
    }
};