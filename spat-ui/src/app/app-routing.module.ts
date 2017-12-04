import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {LoginComponent} from './core/components/login/login.component';
import {PageNotFoundComponent} from './core/components/page-not-found/page-not-found.component';
import {RouteGuardService} from './core/services/route-guard.service';
// import {PANEL_ROUTE_CONFIG, PanelEnum} from './core/services/panel-route.service';
// import {RouteGuardService} from './core/services/route-guard.service';

const routes: Routes = [
  // Feature Routes
  // {
  //   path: PANEL_ROUTE_CONFIG[PanelEnum.AdminTools]['route'],
  //   canActivate: [RouteGuardService],
  //   loadChildren: 'app/features/admin-tools/admin-tools.module#AdminToolsModule',
  //   data: {panelRouteDetails: PANEL_ROUTE_CONFIG[PanelEnum.AdminTools]}
  // },
  // {
  //   path: PANEL_ROUTE_CONFIG[PanelEnum.InfectionControlAssistant]['route'],
  //   canActivate: [RouteGuardService],
  //   loadChildren: 'app/features/ica/ica.module#IcaModule',
  //   data: {panelRouteDetails: PANEL_ROUTE_CONFIG[PanelEnum.InfectionControlAssistant]}
  // },
  // {
  //   path: PANEL_ROUTE_CONFIG[PanelEnum.PatientSearch]['route'],
  //   canActivate: [RouteGuardService],
  //   loadChildren: 'app/features/patient-search/patient-search.module#PatientSearchModule',
  //   data: {panelRouteDetails: PANEL_ROUTE_CONFIG[PanelEnum.PatientSearch]}
  // },
  // {
  //   path: PANEL_ROUTE_CONFIG[PanelEnum.UserSettings]['route'],
  //   canActivate: [RouteGuardService],
  //   loadChildren: 'app/features/user-settings/user-settings.module#UserSettingsModule',
  //   data: {panelRouteDetails: PANEL_ROUTE_CONFIG[PanelEnum.UserSettings]}
  // },
  // Application Routes
  // {path: 'login', canActivate: [RouteGuardService], component: LoginComponent, data: {panelRouteDetails: {title: 'Login'}}},

  {path: 'login', canActivate: [RouteGuardService], component: LoginComponent, data: {panelRouteDetails: {title: 'Login'}}},
  {path: '', redirectTo: 'login', pathMatch: 'full'},
  {path: '**', component: PageNotFoundComponent, data: {panelRouteDetails: {title: 'Page Not Found'}}}
];

@NgModule({
  imports: [
    RouterModule.forRoot(routes)
  ],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
