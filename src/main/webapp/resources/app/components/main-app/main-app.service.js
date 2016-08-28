define([
    "angular"
], function(angular) {
    "use strict";

    mainAppService.$inject = ["modalService"];

    // This service can be used to handle common things across the application
    function mainAppService(modalService) {

        this.showErrorModal = function(message, response) {
            if (response) {
                console.error(response);
            }
            modalService.showModal({
                resolve: {
                    modalData: function() {
                        return {
                            title: "ERROR",
                            message: message
                        };
                    }
                }
            });
        };
    }

    return mainAppService;
});
