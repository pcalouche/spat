import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { ClientUser } from '@core/model/ClientUser';
import { UserSessionService } from '@core/services/user-session.service';

@Component({
  selector: 'app-nav',
  templateUrl: './nav.component.html',
  styleUrls: ['./nav.component.scss']
})
export class NavComponent implements OnInit {
  collapsed = true;
  loggedInUser: ClientUser = null;

  constructor(private userSessionService: UserSessionService,
              private router: Router) {
  }

  ngOnInit() {
    // Subscribe to changes in the logged in user
    this.userSessionService.getLoggedInUserAsObservable().subscribe(loggedInUser => {
      this.loggedInUser = loggedInUser;
    });
  }

  handleLogoutClick() {
    this.userSessionService.clearSession();
    this.router.navigate(['/login']);
  }
}
