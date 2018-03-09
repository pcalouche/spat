import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LoginComponent } from '@core/components/login/login.component';
import { PageNotFoundComponent } from '@core/components/page-not-found/page-not-found.component';
import { RouteGuardService } from '@core/services/route-guard.service';

const routes: Routes = [
  // Feature Routes
  {
    path: 'teams',
    canActivate: [RouteGuardService],
    loadChildren: 'app/features/team/team.module#TeamModule',
    data: {title: 'Teams'}
  },
  {
    path: 'users',
    canActivate: [RouteGuardService],
    loadChildren: 'app/features/user/user.module#UserModule',
    data: {title: 'Users'}
  },
  // Application Routes
  {path: 'login', canActivate: [RouteGuardService], component: LoginComponent, data: {title: 'Login'}},
  {path: '', redirectTo: 'login', pathMatch: 'full'},
  {path: '**', component: PageNotFoundComponent, canActivate: [RouteGuardService], data: {title: 'Page Not Found'}}
];

@NgModule({
  imports: [
    RouterModule.forRoot(routes)
  ],
  exports: [RouterModule]
})
export class AppRoutingModule {
}
