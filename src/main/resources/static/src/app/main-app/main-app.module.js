import angular from "angular";
import {mainAppComponent} from "./main-app.component";
import {mainAppService} from "./main-app.service";

export const mainAppModule = angular.module("app.main-app", [])
    .component("mainApp", mainAppComponent)
    .service("mainAppService", mainAppService);