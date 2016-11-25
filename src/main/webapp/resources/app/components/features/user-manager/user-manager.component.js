define([
    "angular",
], function(angular) {
    "use strict";

    var userManagerComponent = {
        templateUrl: ["spatGlobals", function(spatGlobals) {
            return spatGlobals.featuresRoot + "/user-manager/user-manager.component.html"
        }],
        controller: UserManagerController,
        controllerAs: "vm"
    };

    UserManagerController.$inject = ["mainAppService", "modalService", "UserResource"];

    function UserManagerController(mainAppService, modalService, UserResource) {
        var vm = this;
        vm.users = [];

        vm.addUser = function() {
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
                    vm.users.push(response);
                }, function(response) {
                    mainAppService.showErrorModal("Unable to add user.", response);
                });
            });
        };

        vm.editUser = function(user) {
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
                    vm.users[vm.users.indexOf(user)] = response;
                }, function(response) {
                    mainAppService.showErrorModal("Unable to edit user.", response);
                });
            });
        };

        vm.deleteUser = function(user) {
            modalService.showModal({
                resolve: {
                    modalData: function() {
                        return {
                            title: "Delete User",
                            message: "Are you sure you want to delete " + user.name + "?",
                            includeCancelButton: true
                        };
                    }
                }
            }).then(function() {
                UserResource.delete({id: user.id}, function() {
                    vm.users.splice(vm.users.indexOf(user), 1);
                }, function(response) {
                    mainAppService.showErrorModal("Unable to delete user.", response);
                });
            })
        };

        vm.$onInit = function() {
            UserResource.query(function(response) {
                vm.users = response;
            }, function(response) {
                mainAppService.showErrorModal("Unable to retrieve users.", response);
            });
        };
    }

    return userManagerComponent;
});
