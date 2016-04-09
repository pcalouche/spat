define([], function() {
    "use strict";

    MainNavigationController.$inject = [];

    function MainNavigationController() {
        var vm = this;
        vm.navbarCollapsed = true;
    }

    return MainNavigationController;
});
