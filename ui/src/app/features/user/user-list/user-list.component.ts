import { Component, OnInit } from '@angular/core';
import { UserSessionService } from '@core/services/user-session.service';
import { UserModalComponent } from '@features/user/user-list/user-modal/user-modal.component';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { User } from '@rest-services/api/model/user.model';
import { UserService } from '@rest-services/api/user/user.service';

@Component({
  selector: 'app-user-list',
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.scss']
})
export class UserListComponent implements OnInit {
  users: User[] = [];

  constructor(public userSessionService: UserSessionService,
              private userService: UserService,
              private modalService: NgbModal) {
  }

  ngOnInit() {
    this.userService.users().subscribe(
      users => {
        this.users = users;
      }
    );
  }

  handleAddClick() {
    const modalRef = this.modalService.open(UserModalComponent);
    modalRef.componentInstance.mode = 'add';
    modalRef.componentInstance.user = {
      username: null,
      accountExpired: false,
      accountLocked: false,
      credentialsExpired: false,
      enabled: true,
      authorities: ['ROLE_USER']
    };
    modalRef.result.then(
      (savedUser) => {
        this.users.push(savedUser);
      },
      () => {
      }
    );
  }

  handleEditClick(user: User) {
    const modalRef = this.modalService.open(UserModalComponent);
    modalRef.componentInstance.mode = 'edit';
    modalRef.componentInstance.user = user;
    modalRef.result.then(
      (savedUser) => {
        for (let i = 0; i < this.users.length; i++) {
          if (this.users[i].id === user.id) {
            this.users[i] = savedUser;
          }
        }
      },
      () => {
      }
    );
  }

  handleDeleteClick(user: User) {
    const modalRef = this.modalService.open(UserModalComponent);
    modalRef.componentInstance.mode = 'delete';
    modalRef.componentInstance.user = user;
    modalRef.result.then(
      () => {
        for (let i = 0; i < this.users.length; i++) {
          if (this.users[i].id === user.id) {
            this.users.splice(i, 1);
            break;
          }
        }
      },
      () => {
      }
    );
  }
}
