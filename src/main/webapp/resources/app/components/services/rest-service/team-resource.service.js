define([], function() {
    "use strict";

    TeamResource.$inject = ["$resource", "spatGlobals"];

    function TeamResource($resource, spatGlobals) {
        return $resource(spatGlobals.restServiceUrl + "team/:id");
    }

    return TeamResource;
});