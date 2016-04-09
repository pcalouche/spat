define([
    "angular",
    "./modal.service",
    "./rest-service/rest-service.module"
], function(angular, modalService, restServiceModule) {
    "use strict";

    return angular.module("app.services", [restServiceModule.name])
        .service("modalService", modalService);
});