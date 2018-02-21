import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { ClientUser } from '@core/model/ClientUser';
import { SessionManagementService } from '@core/services/session-management.service';

@Component({
  selector: 'app-nav',
  templateUrl: './nav.component.html',
  styleUrls: ['./nav.component.scss']
})
export class NavComponent implements OnInit {
  collapsed = true;
  loggedInUser: ClientUser = null;

  constructor(private sessionManagementService: SessionManagementService,
              private router: Router) {
  }

  ngOnInit() {
    this.sessionManagementService.getLoggedInUser().subscribe(loggedInUser => {
      this.loggedInUser = loggedInUser;
    });
  }

  handleLogoutClick() {
    this.sessionManagementService.clearSession();
    this.router.navigate(['/login']);
  }
}
