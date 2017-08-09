import angular from "angular";
import {TeamResource} from "./team-resource.service";
import {UserResource} from "./user-resource.service.js";

export const restServiceModule = angular.module("app.restService", [])
    .service("TeamResource", TeamResource)
    .service("UserResource", UserResource);