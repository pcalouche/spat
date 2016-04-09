define([], function() {
    "use strict";

    BasicModalController.$inject = ["$uibModalInstance", "modalData"];

    function BasicModalController($uibModalInstance, modalData) {
        var vm = this;
        vm.title = modalData.title;
        vm.message = modalData.message;
        vm.includeCancelButton = modalData.includeCancelButton ? modalData.includeCancelButton : false;
        vm.okText = modalData.okText ? modalData.okText : "Ok";
        vm.cancelText = modalData.cancelText ? modalData.cancelText : "Cancel";

        vm.ok = function() {
            $uibModalInstance.close();
        };

        vm.cancel = function() {
            $uibModalInstance.dismiss("cancel");
        };
    }

    return BasicModalController;
});