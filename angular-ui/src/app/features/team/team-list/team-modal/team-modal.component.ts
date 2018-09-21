import {Component, Input, OnInit}           from '@angular/core';
import {FormControl, FormGroup, Validators} from '@angular/forms';
import {NgbActiveModal}                     from '@ng-bootstrap/ng-bootstrap';
import {Team}                               from '@rest-services/api/model/team.model.a';
import {TeamService}                        from '@rest-services/api/team/team.service';

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
        // this.actionButtonText = 'Saving Team';
        // this.actionInProgress = true;
        // const teamToSave: Team = {
        //   id: this.team.id,
        //   name: this.teamForm.value.name
        // };
        this.handleActionStart('Saving Team');
        this.teamService.create(this.getFormData()).subscribe(
          (savedTeam) => {
            this.activeModal.close(savedTeam);
          },
          () => {
            this.handleBadSave();
            // this.actionButtonText = 'Save Team';
            // this.actionInProgress = false;
            // this.hideErrorMessage = false;
            // this.errorMessage = 'Unable to save ' + team.name + '.  Please try again later.';
          }
        );
        break;
      case 'edit':
        this.handleActionStart('Saving Team');
        // this.actionButtonText = 'Saving Team';
        // this.actionInProgress = true;
        // const teamToSave: Team = {
        //   id: this.team.id,
        //   name: this.teamForm.value.name
        // };
        this.teamService.create(this.getFormData()).subscribe(
          (savedTeam) => {
            this.activeModal.close(savedTeam);
          },
          () => {
            this.handleBadSave();
            // this.actionButtonText = 'Save Team';
            // this.actionInProgress = false;
            // this.hideErrorMessage = false;
            // this.errorMessage = 'Unable to save ' + team.name + '.  Please try again later.';
          }
        );
        break;
      case 'delete':
        this.handleActionStart('Deleting Team');
        // this.actionButtonText = 'Deleting Team';
        // this.actionInProgress = true;
        this.teamService.delete(team).subscribe(
          () => {
            this.activeModal.close();
          },
          () => {
            this.handleRestError('Delete Team');
            // this.actionButtonText = 'Delete Team';
            // this.actionInProgress = false;
            // this.hideErrorMessage = false;
            this.errorMessage = 'Unable to delete ' + team.name + '.  Please try again later.';
          });
        break;
    }
  }

  private handleActionStart(message: string): void {
    this.actionButtonText = message;
    this.actionInProgress = true;
  }

  private getFormData(): Team {
    return {
      id: this.team.id,
      name: this.teamForm.value.name
    };
  }

  private handleRestError(buttonText: string) {
    this.actionButtonText = buttonText;
    this.actionInProgress = false;
    this.hideErrorMessage = false;
  }

  private handleBadSave(): void {
    this.handleRestError('Save User');
    // this.actionButtonText = 'Save User';
    // this.actionInProgress = false;
    // this.hideErrorMessage = false;
    this.errorMessage = 'Unable to delete ' + this.teamForm.value.name + '.  Please try again later.';
  }
}
