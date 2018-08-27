import { CommonModule } from '@angular/common';
import { HttpClientModule } from '@angular/common/http';
import { NgModule } from '@angular/core';
import { AuthService } from '@rest-services/api/auth/auth.service';
import { TeamService } from '@rest-services/api/team/team.service';
import { UserService } from '@rest-services/api/user/user.service';

@NgModule({
  imports: [
    CommonModule,
    HttpClientModule
  ],
  providers: [AuthService, TeamService, UserService]
})
export class RestModule {
}
