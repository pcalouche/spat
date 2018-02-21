import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LoginComponent } from '@core/login/login.component';
import { PageNotFoundComponent } from '@core/page-not-found/page-not-found.component';
import { RouteGuardService } from '@core/services/route-guard.service';

const routes: Routes = [
  // Feature Routes
  {
    path: 'teams',
    canActivate: [RouteGuardService],
    loadChildren: 'app/features/team/team.module#TeamModule',
    data: {}
  },
  {
    path: 'users',
    canActivate: [RouteGuardService],
    loadChildren: 'app/features/user/user.module#UserModule',
    data: {}
  },
  // Application Routes
  {path: 'login', canActivate: [RouteGuardService], component: LoginComponent, data: {panelRouteDetails: {title: 'Login'}}},
  {path: '', redirectTo: 'login', pathMatch: 'full'},
  {path: '**', component: PageNotFoundComponent, canActivate: [RouteGuardService], data: {panelRouteDetails: {title: 'Page Not Found'}}}
];

@NgModule({
  imports: [
    RouterModule.forRoot(routes)
  ],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
