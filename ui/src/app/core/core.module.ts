import { CommonModule } from '@angular/common';
import { NgModule, Optional, SkipSelf } from '@angular/core';
import { ReactiveFormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { BasicModalComponent } from '@core/components/basic-modal/basic-modal.component';
import { LoginComponent } from '@core/components/login/login.component';
import { NavComponent } from '@core/components/nav/nav.component';
import { PageNotFoundComponent } from '@core/components/page-not-found/page-not-found.component';
import { UserSessionService } from '@core/services/user-session.service';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { RouteGuardService } from './services/route-guard.service';

@NgModule({
  imports: [
    CommonModule,
    NgbModule,
    ReactiveFormsModule,
    RouterModule
  ],
  declarations: [LoginComponent, NavComponent, PageNotFoundComponent, BasicModalComponent],
  entryComponents: [BasicModalComponent],
  providers: [RouteGuardService, UserSessionService],
  exports: [LoginComponent, NavComponent, PageNotFoundComponent, BasicModalComponent]
})
export class CoreModule {
  /* make sure CoreModule is imported only by one NgModule the AppModule */
  constructor(@Optional() @SkipSelf() parentModule: CoreModule) {
    if (parentModule) {
      throw new Error('CoreModule is already loaded. Import only in AppModule');
    }
  }
}
