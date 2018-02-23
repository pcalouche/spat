import { Component, OnInit } from '@angular/core';
import { User } from '@rest-services/api/model/user.model';
import { UserService } from '@rest-services/api/user/user.service';

@Component({
  selector: 'app-user-list',
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.scss']
})
export class UserListComponent implements OnInit {
  users: User[] = [];

  constructor(private userService: UserService) {
  }

  ngOnInit() {
    this.userService.users().subscribe(
      users => {
        this.users = users;
      }
    );
  }

}
