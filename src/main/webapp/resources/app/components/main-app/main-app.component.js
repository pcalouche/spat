define([], function() {
    "use strict";

    var mainAppComponent = {
        templateUrl: ["spatGlobals", function(spatGlobals) {
            return spatGlobals.componentsRoot + "/main-app/main-app.component.html"
        }],
        controller: MainNavigationController,
        controllerAs: "vm"
    };

    MainNavigationController.$inject = [];

    function MainNavigationController() {
        var vm = this;
        vm.navbarCollapsed = true;
    }

    return mainAppComponent;
});
