import {Component, OnInit}                       from '@angular/core';
import {Title}                                   from '@angular/platform-browser';
import {NavigationEnd, Router, RoutesRecognized} from '@angular/router';
import {ClientUser}                              from '@core/model/ClientUser';
import {UserSessionService}                      from '@core/services/user-session.service';
import {filter, map}                             from 'rxjs/operators';

@Component({
  selector: 'app-nav',
  templateUrl: './nav.component.html',
  styleUrls: ['./nav.component.scss']
})
export class NavComponent implements OnInit {
  collapsed = true;
  loggedInUser: ClientUser = null;

  constructor(private userSessionService: UserSessionService,
              private router: Router,
              private titleService: Title) {
  }

  ngOnInit() {
    // Subscribe to changes in the logged in user
    this.userSessionService.getLoggedInUserAsObservable().subscribe(loggedInUser => {
      this.loggedInUser = loggedInUser;
    });

    this.router.events.pipe(
      filter(event => event instanceof RoutesRecognized),
      map((event: RoutesRecognized) => {
        return event.state.root.firstChild.data;
      }))
      .subscribe(data => {
        let fullTitle = 'SPAT Angular UI';
        if (data && data.title) {
          fullTitle += ' - ' + data.title;
        }
        this.titleService.setTitle(fullTitle);
      });

    this.router.events
      .subscribe((event) => {
        if (event instanceof NavigationEnd) {
          this.collapsed = true;
        }
      });
  }

  handleLogoutClick() {
    this.userSessionService.clearSession();
    this.router.navigate(['/login']);
  }
}
