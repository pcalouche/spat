import { Component, HostListener, OnInit } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent implements OnInit {
  // How often to check the session in seconds
  private readonly inactivityCheckInterval: number = 10;
  private lastActivity: Date = new Date();

  ngOnInit(): void {
    this.monitorUserInactivity();
  }

  /**
   * Method to monitor the user's inactivity
   */
  private monitorUserInactivity() {
    console.log('Last Activity->' + this.lastActivity);
    
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

}
