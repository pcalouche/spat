define([
    "angular",
    "./team-resource.service",
    "./user-resource.service"
], function(angular, TeamResource, UserResource) {
    "use strict";

    return angular.module("app.restService", [])
        .service("TeamResource", TeamResource)
        .service("UserResource", UserResource);
});