import { Component, Input, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
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
      id: new FormControl(this.user.id, []),
      username: new FormControl(this.user.username, [Validators.required])
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
        this.userService.saveUser(this.userForm.value).subscribe(
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
