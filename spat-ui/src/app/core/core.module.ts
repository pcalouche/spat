import {CommonModule} from '@angular/common';
import {NgModule} from '@angular/core';
import {ReactiveFormsModule} from '@angular/forms';
import {RouterModule} from '@angular/router';
import {NgbModule} from '@ng-bootstrap/ng-bootstrap';
import {LoginComponent} from './components/login/login.component';
import {PageNotFoundComponent} from './components/page-not-found/page-not-found.component';
import {RouteGuardService} from './services/route-guard.service';
import {TokenService} from './services/token.service';

@NgModule({
  imports: [
    CommonModule,
    NgbModule,
    ReactiveFormsModule,
    RouterModule
  ],
  declarations: [PageNotFoundComponent, LoginComponent],
  providers: [RouteGuardService, TokenService],
  exports: [PageNotFoundComponent]
})
export class CoreModule {
}
