define([], function() {
    "use strict";

    var basicModalComponent = {
        templateUrl: "resources/app/components/general-components/basic-modal/basic-modal.html",
        controller: BasicModalController,
        bindings: {
            modalInstance: "<",
            resolve: "<"
        },
        controllerAs: "vm"
    };

    BasicModalController.$inject = [];

    function BasicModalController() {
        var vm = this;
        var modalData = vm.resolve.modalData;
        vm.title = modalData.title;
        vm.message = modalData.message;
        vm.includeCancelButton = modalData.includeCancelButton ? modalData.includeCancelButton : false;
        vm.okText = modalData.okText ? modalData.okText : "Ok";
        vm.cancelText = modalData.cancelText ? modalData.cancelText : "Cancel";

        vm.ok = function() {
            vm.modalInstance.close();
        };

        vm.cancel = function() {
            vm.modalInstance.dismiss({$value: "cancel"});
        };
    }

    return basicModalComponent;
});
