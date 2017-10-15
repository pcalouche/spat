import {NgModule} from '@angular/core';
import {FormsModule} from '@angular/forms';
import {BrowserModule} from '@angular/platform-browser';

import {AppComponent} from './app.component';
import {TeamManagerComponent} from './components/team-manager/team-manager.component';
import {UserManagerComponent} from './components/user-manager/user-manager.component';


@NgModule({
  declarations: [
    AppComponent,
    TeamManagerComponent,
    UserManagerComponent
  ],
  imports: [
    BrowserModule,
    FormsModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule {
}
