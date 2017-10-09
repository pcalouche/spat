import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { TeamManagerComponent } from './components/team-manager/team-manager.component';
import { UserManagerComponent } from './components/user-manager/user-manager.component';
import { TeamManagerModalComponent } from './components/team-manager/team-manager-modal.component';

@NgModule({
  declarations: [
    AppComponent,
    TeamManagerComponent,
    UserManagerComponent,
    TeamManagerModalComponent
  ],
  imports: [
    BrowserModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
