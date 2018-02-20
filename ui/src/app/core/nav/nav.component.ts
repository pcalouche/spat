import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { SessionManagementService } from '@core/services/session-management.service';

@Component({
  selector: 'app-nav',
  templateUrl: './nav.component.html',
  styleUrls: ['./nav.component.scss']
})
export class NavComponent implements OnInit {
  public collapsed = false;
  public loggedIn = false;

  constructor(public sessionManagementService: SessionManagementService,
              private router: Router) {
  }

  ngOnInit() {
  }

  handleLogoutClick() {
    console.log('will handle logout');
    this.sessionManagementService.clearSession();
    this.router.navigate(['/login']);
  }
}
