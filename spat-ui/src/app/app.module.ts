import {HTTP_INTERCEPTORS} from '@angular/common/http';
import {ErrorHandler, NgModule} from '@angular/core';
import {BrowserModule} from '@angular/platform-browser';
import {NgbModule} from '@ng-bootstrap/ng-bootstrap';
import {AppRoutingModule} from './app-routing.module';
import {AppComponent} from './app.component';
import {CoreModule} from './core/core.module';
import {CustomErrorHandler} from './core/custom-error-handler';
import {CustomHttpInterceptor} from './core/interceptors/custom-http.interceptor';

@NgModule({
  imports: [
    BrowserModule,
    NgbModule.forRoot(),
    CoreModule,
    AppRoutingModule
  ],
  declarations: [
    AppComponent
  ],
  providers: [
    {provide: HTTP_INTERCEPTORS, useClass: CustomHttpInterceptor, multi: true},
    {provide: ErrorHandler, useClass: CustomErrorHandler}
  ],
  bootstrap: [AppComponent]
})
export class AppModule {
}
