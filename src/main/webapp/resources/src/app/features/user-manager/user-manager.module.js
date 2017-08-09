import angular from "angular";
import {userManagerComponent} from "./user-manager.component";
import {userManagerModalComponent} from "./user-manager-modal.component";

moduleConfig.$inject = ["$stateProvider"];

function moduleConfig($stateProvider) {
    $stateProvider.state("user-manager", {
        url: "/user-manager",
        template: "<user-manager></user-manager>"
    });
}

export const userManagerModule = angular.module("app.user-manager", [])
    .config(moduleConfig)
    .component("userManager", userManagerComponent)
    .component("userManagerModal", userManagerModalComponent);