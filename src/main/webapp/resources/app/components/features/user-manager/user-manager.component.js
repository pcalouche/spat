define([
    "angular",
], function(angular) {
    "use strict";

    var userManagerComponent = {
        templateUrl: "resources/app/components/features/user-manager/user-manager.component.html",
        controller: UserManagerController,
        controllerAs: "vm"
    };

    UserManagerController.$inject = ["modalService", "UserResource"];

    function UserManagerController(modalService, UserResource) {
        var vm = this;
        vm.users = [];

        vm.addUser = function() {
            modalService.showModal({
                templateUrl: "resources/app/components/features/user-manager/user-manager-modal.html",
                controller: "UserManagerModalController",
                resolve: {
                    modalData: {
                        action: "Add",
                        user: new UserResource()
                    }
                }
            }).then(function(newUser) {
                UserResource.save(newUser, function(response) {
                    vm.users.push(response);
                });
            });
        };

        vm.editUser = function(user) {
            modalService.showModal({
                templateUrl: "resources/app/components/features/user-manager/user-manager-modal.html",
                controller: "UserManagerModalController",
                resolve: {
                    modalData: {
                        action: "Edit",
                        user: angular.copy(user)
                    }
                }
            }).then(function(updatedUser) {
                UserResource.save(updatedUser, function(response) {
                    vm.users[vm.users.indexOf(user)] = response;
                });
            });
        };

        vm.deleteUser = function(user) {
            modalService.showModal({
                resolve: {
                    modalData: {
                        title: "Delete User",
                        message: "Are you sure you want to delete " + user.name + "?",
                        includeCancelButton: true
                    }
                }
            }).then(function() {
                UserResource.delete({id: user.id}, function() {
                    vm.users.splice(vm.users.indexOf(user), 1);
                });
            })
        };

        function activate() {
            UserResource.query(function(response) {
                vm.users = response;
            }, function(response) {
                alert("Unable to retrieve users.  Response code:" + response.status);
            });
        }

        activate();
    }

    return userManagerComponent;
});
