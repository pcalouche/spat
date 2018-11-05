import {HttpErrorResponse}                             from '@angular/common/http';
import {Component, Input, OnInit}                      from '@angular/core';
import {FormArray, FormControl, FormGroup, Validators} from '@angular/forms';
import {NgbActiveModal}                                from '@ng-bootstrap/ng-bootstrap';
import {User}                                          from '@rest-services/api/model/user.model';
import {UserService}                                   from '@rest-services/api/user/user.service';

@Component({
  selector: 'app-user-modal',
  templateUrl: './user-modal.component.html',
  styleUrls: ['./user-modal.component.scss']
})
export class UserModalComponent implements OnInit {
  @Input() mode: string;
  @Input() user: User;
  title: string;
  actionButtonText: string;
  actionInProgress = false;
  hideErrorMessage = true;
  errorMessage: string;
  userForm: FormGroup;

  roleTypes = [{
    displayName: 'User',
    value: 'ROLE_USER'
  }, {
    displayName: 'Admin',
    value: 'ROLE_ADMIN'
  }];

  constructor(public activeModal: NgbActiveModal,
              private userService: UserService) {
  }

  ngOnInit() {
    switch (this.mode) {
      case 'add':
        this.title = 'Add User';
        this.actionButtonText = 'Save User';
        break;
      case 'edit':
        this.title = 'Edit User';
        this.actionButtonText = 'Save User';
        break;
      case 'delete':
        this.title = 'Confirm Delete';
        this.actionButtonText = 'Delete User';
        break;
    }

    this.userForm = new FormGroup({
      username: new FormControl({value: this.user.username, disabled: this.mode === 'edit'}, [Validators.required]),
      accountExpired: new FormControl(!this.user.accountNonExpired),
      accountLocked: new FormControl(!this.user.accountNonLocked),
      credentialsExpired: new FormControl(!this.user.credentialsNonExpired),
      enabled: new FormControl(this.user.enabled),
      roles: new FormArray([
        new FormControl({
          value: this.user.roles.filter(role => role.name === 'ROLE_USER').length === 1,
          disabled: true
        }),
        new FormControl(this.user.roles.filter(role => role.name === 'ROLE_ADMIN').length === 1)
      ])
    });
  }

  handleActionButtonClick(user: User) {
    this.actionInProgress = true;
    switch (this.mode) {
      case 'add':
        this.handleActionStart('Saving User');
        this.userService.create(this.getFormData()).subscribe(
          (savedUser) => {
            this.activeModal.close(savedUser);
          },
          (response: HttpErrorResponse) => {
            this.handleBadSave(response);
          }
        );
        break;
      case 'edit':
        this.handleActionStart('Saving User');
        this.userService.update(this.getFormData()).subscribe(
          (savedUser) => {
            this.activeModal.close(savedUser);
          },
          (response: HttpErrorResponse) => {
            this.handleBadSave(response);
          }
        );
        break;
      case 'delete':
        this.handleActionStart('Deleting User');
        this.userService.delete(user).subscribe(
          () => {
            this.activeModal.close();
          },
          () => {
            this.handleRestError('Delete User');
            this.errorMessage = 'Unable to delete ' + this.userForm.value.username + '.  Please try again later.';
          });
        break;
    }
  }

  private handleActionStart(message: string): void {
    this.actionButtonText = message;
    this.actionInProgress = true;
  };

  private getFormData(): User {
    const roles: [{id: number, name: string}] = [{id: 1, name: 'ROLE_USER'}];
    // Offset starts at 0 because User checkbox is disabled and that value does not get included in the form
    if (this.userForm.value.roles[0]) {
      roles.push({id: 2, name: 'ROLE_ADMIN'});
    }

    return {
      username: this.mode !== 'edit' ? this.userForm.value.username : this.userForm.controls['username'].value,
      accountNonExpired: !this.userForm.value.accountExpired,
      accountNonLocked: !this.userForm.value.accountLocked,
      credentialsNonExpired: !this.userForm.value.credentialsExpired,
      enabled: this.userForm.value.enabled,
      roles: roles
    };
  };

  private handleRestError(buttonText: string) {
    this.actionButtonText = buttonText;
    this.actionInProgress = false;
    this.hideErrorMessage = false;
  }

  private handleBadSave(response: HttpErrorResponse): void {
    this.handleRestError('Save User');
    if (response.status === 0) {
      this.errorMessage = 'Unable to save ' + this.userForm.value.username + '.  Please try again later.';
    } else {
      this.errorMessage = response.error.message;
    }
  }
}
