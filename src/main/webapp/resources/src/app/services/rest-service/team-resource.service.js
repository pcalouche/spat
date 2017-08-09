TeamResource.$inject = ["$resource", "appGlobals"];

export function TeamResource($resource, appGlobals) {
    return $resource(appGlobals.contextPath + "team/:id");
}