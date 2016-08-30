require.config({
    // This is only for development, so that nothing is cached by RequireJS
    urlArgs: "bust=" + (new Date()).getTime(),
    baseUrl: "resources/app",
    paths: {
        // AngularJS and Angular Core modules
        angular: "lib/angular/1.5.8/angular",
        ngAnimate: "lib/angular/1.5.8/angular-animate",
        ngResource: "lib/angular/1.5.8/angular-resource",
        // AngularUI Module
        uiBootstrap: "lib/angular-ui/ui-bootstrap/2.1.3/ui-bootstrap-tpls-2.1.3",
        uiRouter: "lib/angular-ui/ui-router/0.3.1/angular-ui-router",
        // Other Angular Modules
        smartTable: "lib/angular-other/smart-table/2.1.8/smart-table"
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
