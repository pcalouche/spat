import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, CanActivateChild, Router, RouterStateSnapshot } from '@angular/router';
import { UserSessionService } from '@core/services/user-session.service';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class RouteGuardService implements CanActivate, CanActivateChild {
  constructor(private userSessionService: UserSessionService,
              private router: Router) {
  }

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> | Promise<boolean> | boolean {
    const url = state.url;
    // Handle cases where user is logged
    if (this.userSessionService.getLoggedInUser()) {
      if (!url.startsWith('/login')) {
        // Additional permissions could be evaluated here
        return true;
      } else {
        // If user tries to go to the login page but are already logged in send them to the teams page by default
        this.router.navigate(['/teams']);
        return false;
      }
    } else { // User not logged in cases
      if (!url.startsWith('/login')) {
        // If not logged in and user goes to a non-login page redirect them to login
        // The redirect will be evaluated by the login component and send the user to the
        // page the user wanted to go to in the first place after successful login.
        this.router.navigate(['/login', {redirect: state.url}]);
        return false;
      } else {
        // If not logged in and user goes to the login page then let them
        return true;
      }
    }
  }

  canActivateChild(childRoute: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> | Promise<boolean> | boolean {
    return this.canActivate(childRoute, state);
  }
}
