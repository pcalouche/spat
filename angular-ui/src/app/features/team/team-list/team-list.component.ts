import {Component, OnInit}       from '@angular/core';
import {UserSessionService}      from '@core/services/user-session.service';
import {TeamModalComponent}      from '@features/team/team-list/team-modal/team-modal.component';
import {faPencilAlt, faTrashAlt} from '@fortawesome/free-solid-svg-icons';
import {NgbModal}                from '@ng-bootstrap/ng-bootstrap';
import {Team}                    from '@rest-services/api/model/team.model.a';
import {TeamService}             from '@rest-services/api/team/team.service';

@Component({
  selector: 'app-team-list',
  templateUrl: './team-list.component.html',
  styleUrls: ['./team-list.component.scss']
})
export class TeamListComponent implements OnInit {
  teams: Team[] = [];
  faPencilAlt = faPencilAlt;
  faTrashAlt = faTrashAlt;

  constructor(public userSessionService: UserSessionService,
              private teamService: TeamService,
              private modalService: NgbModal) {
  }

  ngOnInit() {
    this.teamService.teams().subscribe(
      teams => {
        this.teams = teams;
      }
    );
  }

  handleAddClick() {
    const modalRef = this.modalService.open(TeamModalComponent);
    modalRef.componentInstance.mode = 'add';
    modalRef.componentInstance.team = {};
    modalRef.result.then(
      (savedTeam) => {
        this.teams.push(savedTeam);
      },
      () => {
      }
    );
  }

  handleEditClick(team: Team) {
    const modalRef = this.modalService.open(TeamModalComponent);
    modalRef.componentInstance.mode = 'edit';
    modalRef.componentInstance.team = team;
    modalRef.result.then(
      (savedTeam) => {
        for (let i = 0; i < this.teams.length; i++) {
          if (this.teams[i].id === team.id) {
            this.teams[i] = savedTeam;
          }
        }
      },
      () => {
      }
    );
  }

  handleDeleteClick(team: Team) {
    const modalRef = this.modalService.open(TeamModalComponent);
    modalRef.componentInstance.mode = 'delete';
    modalRef.componentInstance.team = team;
    modalRef.result.then(
      () => {
        for (let i = 0; i < this.teams.length; i++) {
          if (this.teams[i].id === team.id) {
            this.teams.splice(i, 1);
            break;
          }
        }
      },
      () => {
      }
    );
  }
}
