require.config({
    waitSeconds: 30,
    baseUrl: "resources/app",
    paths: {
        // AngularJS and Angular Core modules
        angular: "lib/angular/1.6.1/angular",
        ngAnimate: "lib/angular/1.6.1/angular-animate",
        ngResource: "lib/angular/1.6.1/angular-resource",
        // AngularUI Modules
        uiBootstrap: "lib/angular-ui/ui-bootstrap/2.5.0/ui-bootstrap-tpls-2.5.0",
        uiRouter: "lib/angular-ui/ui-router/0.4.2/angular-ui-router"
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
