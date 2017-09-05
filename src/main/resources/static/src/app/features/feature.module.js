import angular from "angular";
import {teamManagerModule} from "./team-manager/team-manager.module";
import {userManagerModule} from "./user-manager/user-manager.module";

export const featuresModule = angular.module("app.features", [
    teamManagerModule.name,
    userManagerModule.name
]);