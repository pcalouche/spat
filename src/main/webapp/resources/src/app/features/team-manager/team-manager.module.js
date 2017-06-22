import angular from "angular";
import {teamManagerComponent} from "./team-manager.component";
import {teamManagerModalComponent} from "./team-manager-modal.component";

moduleConfig.$inject = ["$stateProvider"];

function moduleConfig($stateProvider) {
    $stateProvider.state("team-manager", {
        url: "/team-manager",
        template: "<team-manager></team-manager>"
    });
}

export const teamManagerModule = angular.module("app.team-manager", [])
    .config(moduleConfig)
    .component("teamManager", teamManagerComponent)
    .component("teamManagerModal", teamManagerModalComponent);