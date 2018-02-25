import { Component, OnInit } from '@angular/core';
import { BasicModalComponent, BasicModalConfig } from '@core/components/basic-modal/basic-modal.component';
import { UserSessionService } from '@core/services/user-session.service';
import { Team } from '@rest-services/api/model/team.model.a';
import { TeamService } from '@rest-services/api/team/team.service';
import { NgbModal } from '../../../../../node_modules/@ng-bootstrap/ng-bootstrap';

@Component({
  selector: 'app-team-list',
  templateUrl: './team-list.component.html',
  styleUrls: ['./team-list.component.scss']
})
export class TeamListComponent implements OnInit {
  teams: Team[] = [];

  constructor(private userSessionService: UserSessionService,
              private teamService: TeamService,
              private modalService: NgbModal) {
  }

  ngOnInit() {
    this.teamService.teams().subscribe(
      teams => {
        this.teams = teams;
      }
    );
    console.info(this.userSessionService.getLoggedInUser());
  }

  handleEditClick(team: Team) {

  }

  handleDeleteClick(team: Team) {
    const deleteModalConfig: BasicModalConfig = {
      type: 'confirm',
      title: 'Delete Confirmation',
      message: 'Are you sure you want to delete ' + team.name + '?'
    };

    const modalRef = this.modalService.open(BasicModalComponent);
    modalRef.componentInstance.config = deleteModalConfig;

    modalRef.result.then(
      () => {
        this.teamService.deleteTeam(team).subscribe(
          () => {
            for (let i = 0; i < this.teams.length; i++) {
              if (this.teams[i].id == team.id) {
                this.teams.splice(i, 1);
                break;
              }
            }
          }, () => {
            const failedDeleteModalConfig: BasicModalConfig = {
              type: 'error',
              title: 'Delete failure',
              message: 'Failed to team ' + team.name + '.'
            };
            const modalRef = this.modalService.open(BasicModalComponent);
            modalRef.componentInstance.config = failedDeleteModalConfig;
          }
        );
      }, () => {
      });
  }
}
