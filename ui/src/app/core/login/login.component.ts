import { HttpErrorResponse } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { AuthService } from '@app/rest/api/auth/auth.service';
import { RestServiceHelper } from '@app/rest/api/rest-service-helper';
import { UserService } from '@app/rest/api/user/user.service';
import { coreModuleErrors } from '@core/core-messages';
import { ClientUser } from '@core/model/ClientUser';
import { UserSessionService } from '@core/services/user-session.service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit {
  errorMessage: string;
  showErrorMessage: boolean;
  loginForm = new FormGroup({
    username: new FormControl('', [Validators.required]),
    password: new FormControl('', [Validators.required])
  });

  constructor(private authService: AuthService,
              private userService: UserService,
              private userSessionService: UserSessionService,
              private router: Router,
              private activatedRoute: ActivatedRoute) {
  }

  ngOnInit() {
  }

  handleLoginClick() {
    // Clear state before requesting new token
    this.userSessionService.clearSession();
    this.showErrorMessage = false;

    this.authService.token(this.loginForm).subscribe(
      authResponse => {
        this.userSessionService.storeTokens(authResponse);

        // Load the details of the logged in user now
        this.userService.getByUsername(this.loginForm.value.username).subscribe(
          user => {
            this.userSessionService.setLoggedInUser(new ClientUser(user));
            // If coming from a redirect go there, otherwise go to teams page by default
            if (this.activatedRoute.snapshot.params['redirect']) {
              this.router.navigate([this.activatedRoute.snapshot.paramMap.get('redirect')]);
            } else {
              this.router.navigate(['/teams']);
            }
          }, () => {
            this.errorMessage = coreModuleErrors.unknown;
            this.showErrorMessage = true;
            this.userSessionService.clearSession();
          }
        );

      }, (error: HttpErrorResponse) => {
        // If a client code came from there server inspect it for more information
        if (error.error.clientCode) {
          switch (error.error.clientCode) {
            case RestServiceHelper.clientCodes.badCredentials:
              this.errorMessage = coreModuleErrors.loginComponent.badCredentials;
              break;
            case RestServiceHelper.clientCodes.accountExpired:
              this.errorMessage = coreModuleErrors.loginComponent.accountExpired;
              break;
            case RestServiceHelper.clientCodes.accountCredentialsExpired:
              this.errorMessage = coreModuleErrors.loginComponent.accountCredentialsExpired;
              break;
            case RestServiceHelper.clientCodes.accountLocked:
              this.errorMessage = coreModuleErrors.loginComponent.accountLocked;
              break;
            case RestServiceHelper.clientCodes.accountDisabled:
              this.errorMessage = coreModuleErrors.loginComponent.accountDisabled;
              break;
            default:
              this.errorMessage = coreModuleErrors.unknown;
          }
        } else {
          this.errorMessage = coreModuleErrors.unknown;
        }
        this.showErrorMessage = true;
      }
    );
  }
}
