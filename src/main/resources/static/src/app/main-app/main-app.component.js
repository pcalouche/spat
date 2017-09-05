import template from "./main-app.component.html";

MainNavigationController.$inject = [];

function MainNavigationController() {
    let ctrl = this;
    ctrl.navbarCollapsed = true;
}

export const mainAppComponent = {
    template: template,
    controller: MainNavigationController
};