define([], function() {
    "use strict";

    var mainAppComponent = {
        templateUrl: "resources/app/components/main-app/main-app.component.html",
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
