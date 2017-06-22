import angular from "angular";
import template from "./user-manager.component.html";

UserManagerController.$inject = ["mainAppService", "modalService", "UserResource"];

function UserManagerController(mainAppService, modalService, UserResource) {
    let ctrl = this;
    ctrl.users = [];

    ctrl.$onInit = function() {
        UserResource.query(function(response) {
            ctrl.users = response;
        }, function(response) {
            mainAppService.showErrorModal("Unable to retrieve users.", response);
        });
    };

    ctrl.addUser = function() {
        modalService.showModal({
            component: "userManagerModal",
            resolve: {
                modalData: function() {
                    return {
                        action: "Add",
                        user: new UserResource()
                    };
                }
            }
        }).then(function(newUser) {
            UserResource.save(newUser, function(response) {
                ctrl.users.push(response);
            }, function(response) {
                mainAppService.showErrorModal("Unable to add user.", response);
            });
        }, angular.noop);
    };

    ctrl.editUser = function(user) {
        modalService.showModal({
            component: "userManagerModal",
            resolve: {
                modalData: function() {
                    return {
                        action: "Edit",
                        user: angular.copy(user)
                    };
                }
            }
        }).then(function(updatedUser) {
            UserResource.save(updatedUser, function(response) {
                ctrl.users[ctrl.users.indexOf(user)] = response;
            }, function(response) {
                mainAppService.showErrorModal("Unable to edit user.", response);
            });
        }, angular.noop);
    };

    ctrl.deleteUser = function(user) {
        modalService.showModal({
            resolve: {
                modalData: function() {
                    return {
                        title: "Delete User",
                        message: "Are you sure you want to delete " + user.firstName + " " + user.lastName + "?",
                        includeCancelButton: true
                    };
                }
            }
        }).then(function() {
            UserResource.delete({id: user.id}, function() {
                ctrl.users.splice(ctrl.users.indexOf(user), 1);
            }, function(response) {
                mainAppService.showErrorModal("Unable to delete user.", response);
            });
        }, angular.noop);
    };
}

export const userManagerComponent = {
    template: template,
    controller: UserManagerController
};