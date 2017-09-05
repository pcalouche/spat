import "./vendor";
import "font-awesome/css/font-awesome.css";
import "bootstrap/dist/css/bootstrap.css";
import "../assets/css/app.less";
import angular from "angular";
import {commonModule} from "./common/common-module";
import {servicesModule} from "./services/services.module";
import {mainAppModule} from "./main-app/main-app.module";
import {featuresModule} from "./features/feature.module";

interceptors.$inject = [];

function interceptors() {
    // Returns object representing AngularJS http interceptors for app
    return {
        request: function(config) {
            config.headers.AUTH_TOKEN = "myAuthCode";
            return config;
        }
    };
}

appConfig.$inject = ["$httpProvider", "$locationProvider", "$urlRouterProvider"];

function appConfig($httpProvider, $locationProvider, $urlRouterProvider) {
    $httpProvider.interceptors.push(interceptors);
    $locationProvider.html5Mode(true);
    $urlRouterProvider.otherwise("team-manager");

    // In a large project the routes can get quite long.  To avoid a large list, each feature module
    // declares its own config for routes and anything else that may be needed
}

angular.module("app", [
        "ngAnimate",
        "ngResource",
        "ui.bootstrap",
        "ui.router",
        commonModule.name,
        servicesModule.name,
        mainAppModule.name,
        featuresModule.name
    ]
).constant("appGlobals", appGlobals)
    .config(appConfig);

angular.element(document).ready(function() {
    angular.bootstrap(document, ["app"], {
        strictDi: true
    });
});

