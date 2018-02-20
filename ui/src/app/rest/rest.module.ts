import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { AuthService } from '@app/rest/api/auth/auth.service';
import { TeamService } from '@app/rest/api/team/team.service';
import { UserService } from '@app/rest/api/user/user.service';

@NgModule({
  imports: [
    CommonModule
  ],
  providers: [AuthService, TeamService, UserService]
})
export class RestModule {
}
