import { Component, Input, OnInit } from '@angular/core';
import { FormArray, FormControl, FormGroup, Validators } from '@angular/forms';
import { NgbActiveModal } from '@ng-bootstrap/ng-bootstrap';
import { User } from '@rest-services/api/model/user.model';
import { UserService } from '@rest-services/api/user/user.service';

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

  authorityTypes = [{
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
      username: new FormControl(this.user.username, [Validators.required]),
      accountExpired: new FormControl(!this.user.accountNonExpired),
      accountLocked: new FormControl(!this.user.accountNonLocked),
      credentialsExpired: new FormControl(!this.user.credentialsNonExpired),
      enabled: new FormControl(this.user.enabled),
      authorities: new FormArray([
        new FormControl({value: this.user.authorities.indexOf('ROLE_USER') !== -1, disabled: true}),
        new FormControl(this.user.authorities.indexOf('ROLE_ADMIN') !== -1)
      ])
    });
  }

  handleActionButtonClick(user: User) {
    this.actionInProgress = true;
    switch (this.mode) {
      case 'add':
      // fall through is on purpose
      case 'edit':
        this.actionButtonText = 'Saving User';
        this.actionInProgress = true;
        const authorities: string[] = ['ROLE_USER'];
        for (let i = 0; i < this.userForm.value.authorities.length; i++) {
          if (this.userForm.value.authorities[i]) {
            // Offset is because User checkbox is disabled and that value does not get included in the form
            authorities.push(this.authorityTypes[i + 1].value);
          }
        }
        const userToSave: User = {
          id: this.user.id,
          username: this.userForm.value.username,
          accountNonExpired: !this.userForm.value.accountExpired,
          accountNonLocked: !this.userForm.value.accountLocked,
          credentialsNonExpired: !this.userForm.value.credentialsExpired,
          enabled: this.userForm.value.enabled,
          authorities: authorities
        };

        this.userService.saveUser(userToSave).subscribe(
          (savedUser) => {
            this.activeModal.close(savedUser);
          },
          () => {
            this.actionButtonText = 'Save User';
            this.actionInProgress = false;
            this.hideErrorMessage = false;
            this.errorMessage = 'Unable to save ' + this.user.username + '.  Please try again later.';
          }
        );
        break;
      case 'delete':
        this.actionButtonText = 'Deleting User';
        this.actionInProgress = true;
        this.userService.deleteUser(user).subscribe(
          () => {
            this.activeModal.close();
          },
          () => {
            this.actionButtonText = 'Delete User';
            this.actionInProgress = false;
            this.hideErrorMessage = false;
            this.errorMessage = 'Unable to delete ' + user.username + '.  Please try again later.';
          });
        break;
    }
  }

}
