import angular from "angular";
import {basicModalComponent} from "./basic-modal/basic-modal.component";

export const commonModule = angular.module("app.common", [])
    .component("basicModal", basicModalComponent);