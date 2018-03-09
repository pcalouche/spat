import { Component, Input, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { NgbActiveModal } from '@ng-bootstrap/ng-bootstrap';
import { Team } from '@rest-services/api/model/team.model.a';
import { TeamService } from '@rest-services/api/team/team.service';

@Component({
  selector: 'app-team-modal',
  templateUrl: './team-modal.component.html',
  styleUrls: ['./team-modal.component.scss']
})
export class TeamModalComponent implements OnInit {
  @Input() mode: string;
  @Input() team: Team;
  title: string;
  actionButtonText: string;
  actionInProgress = false;
  hideErrorMessage = true;
  errorMessage: string;
  teamForm: FormGroup;

  constructor(public activeModal: NgbActiveModal,
              private teamService: TeamService) {
  }

  ngOnInit() {
    switch (this.mode) {
      case 'add':
        this.title = 'Add Team';
        this.actionButtonText = 'Save Team';
        break;
      case 'edit':
        this.title = 'Edit Team';
        this.actionButtonText = 'Save Team';
        break;
      case 'delete':
        this.title = 'Confirm Delete';
        this.actionButtonText = 'Delete Team';
        break;
    }
    this.teamForm = new FormGroup({
      name: new FormControl(this.team.name, [Validators.required])
    });
  }

  handleActionButtonClick(team: Team) {
    this.actionInProgress = true;
    switch (this.mode) {
      case 'add':
      // fall through is on purpose
      case 'edit':
        this.actionButtonText = 'Saving Team';
        this.actionInProgress = true;
        const teamToSave: Team = {
          id: this.team.id,
          name: this.teamForm.value.name
        };
        this.teamService.saveTeam(teamToSave).subscribe(
          (savedTeam) => {
            this.activeModal.close(savedTeam);
          },
          () => {
            this.actionButtonText = 'Save Team';
            this.actionInProgress = false;
            this.hideErrorMessage = false;
            this.errorMessage = 'Unable to save ' + team.name + '.  Please try again later.';
          }
        );
        break;
      case 'delete':
        this.actionButtonText = 'Deleting Team';
        this.actionInProgress = true;
        this.teamService.deleteTeam(team).subscribe(
          () => {
            this.activeModal.close();
          },
          () => {
            this.actionButtonText = 'Delete Team';
            this.actionInProgress = false;
            this.hideErrorMessage = false;
            this.errorMessage = 'Unable to delete ' + team.name + '.  Please try again later.';
          });
        break;
    }
  }
}
