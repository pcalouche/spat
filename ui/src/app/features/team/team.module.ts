import { NgModule } from '@angular/core';
import { SharedModule } from '@shared/shared.module';
import { TeamRoutingModule } from './team-routing.module';
import { TeamListComponent } from './team-list/team-list.component';
import { TeamDetailsModalComponent } from './team-list/team-details-modal/team-details-modal.component';

@NgModule({
  imports: [
    SharedModule,
    TeamRoutingModule
  ],
  declarations: [TeamListComponent, TeamDetailsModalComponent]
})
export class TeamModule {
}
