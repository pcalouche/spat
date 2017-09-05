mainAppService.$inject = ["modalService"];

export function mainAppService(modalService) {

    this.showErrorModal = function(message, response) {
        if (response) {
            console.error(response);
        }
        if (response.data && response.data.error) {
            console.error("error type->" + response.data.error.type);
            console.error("error message->" + response.data.error.message);
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
