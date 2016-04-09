define([], function() {
    "use strict";

    UserResource.$inject = ["$resource", "spatGlobals"];

    function UserResource($resource, spatGlobals) {
        return $resource(spatGlobals.restServiceUrl + "user/:id");
    }

    return UserResource;
});