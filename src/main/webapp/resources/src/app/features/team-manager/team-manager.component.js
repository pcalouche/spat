import angular from "angular";
import template from "./team-manager.component.html";

TeamManagerController.$inject = ["mainAppService", "modalService", "TeamResource"];

function TeamManagerController(mainAppService, modalService, TeamResource) {
    let ctrl = this;
    ctrl.teams = null;

    ctrl.$onInit = function() {
        TeamResource.query(function(response) {
            ctrl.teams = response;
        }, function(response) {
            mainAppService.showErrorModal("Unable to retrieve teams.", response);
        });
    };

    ctrl.addTeam = function() {
        modalService.showModal({
            component: "teamManagerModal",
            resolve: {
                modalData: function() {
                    return {
                        action: "Add",
                        team: new TeamResource()
                    };
                }
            }
        }).then(function(newTeam) {
            TeamResource.save(newTeam, function(response) {
                ctrl.teams.push(response);
            }, function(response) {
                mainAppService.showErrorModal("Unable to add team.", response);
            });
        }, angular.noop);
    };

    ctrl.editTeam = function(team) {
        modalService.showModal({
            component: "teamManagerModal",
            resolve: {
                modalData: function() {
                    return {
                        action: "Edit",
                        team: angular.copy(team)
                    };
                }
            }
        }).then(function(updatedTeam) {
            TeamResource.save(updatedTeam, function(response) {
                ctrl.teams[ctrl.teams.indexOf(team)] = response;
            }, function(response) {
                mainAppService.showErrorModal("Unable to edit team.", response);
            });
        }, angular.noop);
    };

    ctrl.deleteTeam = function(team) {
        modalService.showModal({
            resolve: {
                modalData: function() {
                    return {
                        title: "Delete Team",
                        message: "Are you sure you want to delete " + team.name + "?",
                        includeCancelButton: true
                    };
                }
            }
        }).then(function() {
            TeamResource.delete({id: team.id}, function() {
                ctrl.teams.splice(ctrl.teams.indexOf(team), 1);
            }, function(response) {
                mainAppService.showErrorModal("Unable to delete team.", response);
            });
        }, angular.noop);
    };
}

export const teamManagerComponent = {
    template: template,
    controller: TeamManagerController
};