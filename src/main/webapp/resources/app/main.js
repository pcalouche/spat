require.config({
    // This is only for development, so that nothing is cached by RequireJS
    urlArgs: "bust=" + (new Date()).getTime(),
    baseUrl: "resources/app",
    paths: {
        // AngularJS and Angular Core modules
        angular: "lib/angular/1.5.0/angular",
        ngAnimate: "lib/angular/1.5.0/angular-animate",
        ngResource: "lib/angular/1.5.0/angular-resource",
        // AngularUI Module
        uiBootstrap: "lib/angular-ui/ui-bootstrap/1.3.1/ui-bootstrap-tpls-1.3.1",
        uiRouter: "lib/angular-ui/ui-router/0.2.18/angular-ui-router",
        // Other Angular Modules
        smartTable: "lib/angular-other/smart-table/2.1.7/smart-table"
        // Convenience paths
    },
    shim: {
        // AngularJS and Angular Core modules
        angular: {
            exports: "angular"
        },
        ngAnimate: {
            deps: ["angular"]
        },
        ngResource: {
            deps: ["angular"]
        },
        // AngularUI Modules
        uiBootstrap: {
            deps: ["angular"]
        },
        uiRouter: {
            deps: ["angular"]
        },
        // Other Angular Modules
        smartTable: {
            deps: ["angular"]
        }
    }
});

require([
    "angular",
    "app.module"
], function(angular, app) {
    /* Bootstrap application after document is ready with strict angular dependency injection.
     This will help ensure the all JavaScript code can be concatenated and uglified correctly.*/
    angular.element(document).ready(function() {
        angular.bootstrap(document.getElementsByTagName("html")[0], ["app"], {
            strictDi: true
        });
    });
});
