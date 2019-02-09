import {NgModule}           from '@angular/core';
import {SharedModule}       from '@shared/shared.module';
import {TeamListComponent}  from './team-list/team-list.component';
import {TeamModalComponent} from './team-list/team-modal/team-modal.component';
import {TeamRoutingModule}  from './team-routing.module';

@NgModule({
  imports: [
    SharedModule,
    TeamRoutingModule
  ],
  declarations: [TeamListComponent, TeamModalComponent],
  entryComponents: [TeamModalComponent]
})
export class TeamModule {
}
