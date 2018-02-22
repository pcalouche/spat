import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { AuthService } from '@app/rest-services/api/auth/auth.service';
import { TeamService } from '@app/rest-services/api/team/team.service';
import { UserService } from '@app/rest-services/api/user/user.service';

@NgModule({
  imports: [
    CommonModule
  ],
  providers: [AuthService, TeamService, UserService]
})
export class RestModule {
}
