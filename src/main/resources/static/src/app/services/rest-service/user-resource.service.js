UserResource.$inject = ["$resource", "appGlobals"];

export function UserResource($resource, appGlobals) {
    return $resource(appGlobals.contextPath + "user/:id");
}