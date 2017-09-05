import angular from "angular";
import {modalService} from "./modal.service";
import {restServiceModule} from "./rest-service/rest-service.module";

export const servicesModule = angular.module("app.services", [
    restServiceModule.name
]).service("modalService", modalService);