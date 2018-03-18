import { Component, HostListener, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { BasicModalComponent, BasicModalConfig } from '@core/components/basic-modal/basic-modal.component';
import { coreModuleMessages } from '@core/core-messages';
import { UserSessionService } from '@core/services/user-session.service';
import { NgbModal, NgbModalRef } from '@ng-bootstrap/ng-bootstrap';
import { AuthService } from '@rest-services/api/auth/auth.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent implements OnInit {
  // How often to check the session in seconds
  private readonly inactivityCheckInterval: number = 10;
  private lastActivity: Date = new Date();
  private modalRef: NgbModalRef;
  private logoutWarningModalConfig: BasicModalConfig = {
    type: 'warning',
    title: 'Logout Warning',
    message: coreModuleMessages.appComponent.logoutWarning
  };
  private loggedOutModalConfig: BasicModalConfig = {
    type: 'info',
    title: 'Logged Out',
    message: coreModuleMessages.appComponent.loggedOut
  };

  constructor(private userSessionService: UserSessionService,
              private authService: AuthService,
              private modalService: NgbModal,
              private router: Router) {
  }

  ngOnInit(): void {
    this.monitorUserInactivity();
  }

  /**
   * Method to monitor the user's inactivity
   */
  private monitorUserInactivity() {
    // If there is a logged in user then monitor if they are still active
    if (this.userSessionService.getLoggedInUser()) {
      const nowTime = new Date().getTime();
      const inactiveTime = (nowTime - this.lastActivity.getTime()) / 1000;
      console.log('inactivity-> ' + inactiveTime + ' seconds');
      const timeLeft = (this.userSessionService.getTokenExpiration().getTime() - nowTime) / 1000;
      console.log('time left-> ' + timeLeft + ' seconds');

      // Refresh the token if it is to expire in the next 60 seconds.  The next set of checks will check for user inactivity.
      // User inactivity is the ultimate indicator when deciding to prompt the user if they want to extend their session or
      // to log them out all together.
      if (timeLeft <= 60) {
        this.refreshToken();
      }

      // If there is less than 60 seconds left before the token expires or if the user has been for the token's
      // duration minus 60 seconds then show a modal letting the user know their session is about to expire.
      if (!this.modalRef && inactiveTime >= this.userSessionService.getTokenDuration() - 60) {
        // Store this in case the user decides to dismiss the modal versus confirming the want to stay logged in
        const tempLastActivity = this.lastActivity;
        this.modalRef = this.modalService.open(BasicModalComponent);
        this.modalRef.componentInstance.config = this.logoutWarningModalConfig;
        this.modalRef.result.then(
          () => {
            console.log('User decided to extend session. The token will be refreshed');
            this.modalRef = null;
            this.refreshToken();
          }, () => {
            // Modal was dismissed, so do not count the click that caused the dismissal as part of the last activity
            console.log('modal was dismissed.  The token will not be refreshed');
            this.modalRef = null;
            this.lastActivity = tempLastActivity;
          });
      }

      // If then token will expire before the next interval check or if the user is likely to still be inactive before the next interval check,
      // then log the user out and display a modal letting them know why they were logged out.  Even though we refresh the token when it is
      // set to expire in the next 60 seconds, it is quite possible we don't get a refresh token if the server is down or there is a network
      // issue that is preventing the client from communicating with the server.
      if (timeLeft <= this.inactivityCheckInterval || this.userSessionService.getTokenDuration() <= (inactiveTime + this.inactivityCheckInterval)) {
        this.userSessionService.clearSession();
        this.router.navigate(['/login']);
        // Dismiss logout warning modal if it is displayed.
        if (this.modalRef) {
          this.modalRef.dismiss();
        }
        this.modalRef = this.modalService.open(BasicModalComponent);
        this.modalRef.componentInstance.config = this.loggedOutModalConfig;
      }

    }

    // Restart timeout for monitoring the login
    setTimeout(() => {
      this.monitorUserInactivity();
    }, this.inactivityCheckInterval * 1000);
  }

  /**
   * Method that is called when an application wide events happens.  This method needs to be fast
   * to avoid unresponsiveness on the UI.  It should also not include events that would happen very
   * frequently.
   */
  @HostListener('document:mousedown')
  @HostListener('document:mouseup')
  handleAppEvent() {
    // Reset the active time
    this.lastActivity = new Date();
  }

  /**
   * Method to refresh the user's token using the refresh token
   */
  private refreshToken() {
    this.authService.refreshToken(this.userSessionService.getRefreshToken()).subscribe(
      response => {
        this.userSessionService.storeTokens(response);
      }
    );
  }
}
