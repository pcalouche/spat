import { Injectable } from '@angular/core';
import { AuthResponse } from '@app/rest/api/model/auth-response.model';
import { ClientUser } from '@core/model/ClientUser';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class SessionManagementService {
  private readonly storageKey: string = 'spatSession';
  private readonly tokenKey: string = 'token';
  private readonly refreshTokenKey: string = 'refreshToken';
  private readonly loggedInUserKey: string = 'loggedInUser';
  private tokenClaims: any;
  private tokenExpiration: Date;
  private tokenDuration: number;
  private loggedInUser = new BehaviorSubject<ClientUser>(null);

  constructor() {
    // Check session storage for session on creation
    if (sessionStorage.getItem(this.storageKey)) {
      console.log('SPAT session found in session storage found');
      this.parseClaims(this.getToken());
      this.loggedInUser.next(new ClientUser(JSON.parse(sessionStorage.getItem(this.storageKey))[this.loggedInUserKey]));
    }
  }

  storeTokens(authResponse: AuthResponse) {
    // Parse the claims
    this.parseClaims(authResponse.token);
    // Store tokens in session storage object
    const spatSession = sessionStorage.getItem(this.storageKey) ? JSON.parse(sessionStorage.getItem(this.storageKey)) : {};
    spatSession[this.tokenKey] = authResponse.token;
    spatSession[this.refreshTokenKey] = authResponse.refreshToken;
    sessionStorage.setItem(this.storageKey, JSON.stringify(spatSession));
  }

  clearSession() {
    this.tokenClaims = null;
    this.tokenExpiration = null;
    this.loggedInUser.next(null);
    sessionStorage.removeItem(this.storageKey);
  }

  getToken(): string {
    return JSON.parse(sessionStorage.getItem(this.storageKey))[this.tokenKey];
  }

  getRefreshToken(): string {
    return JSON.parse(sessionStorage.getItem(this.storageKey))[this.refreshTokenKey];
  }

  getTokenClaims(): any {
    return this.tokenClaims;
  }

  getTokenExpiration(): Date {
    return this.tokenExpiration;
  }

  getTokenDuration(): number {
    return this.tokenDuration;
  }

  getLoggedInUser(): Observable<ClientUser> {
    return this.loggedInUser.asObservable();
  }

  setLoggedInUser(loggedInUser: ClientUser) {
    this.loggedInUser.next(loggedInUser);
    const spatSession = JSON.parse(sessionStorage.getItem(this.storageKey));
    spatSession[this.loggedInUserKey] = JSON.stringify(loggedInUser);
    sessionStorage.setItem(this.storageKey, JSON.stringify(spatSession));
  }

  private parseClaims(token: string) {
    if (token) {
      const base64Url = token.split('.')[1];
      const base64 = base64Url.replace('-', '+').replace('_', '/');
      this.tokenClaims = JSON.parse(window.atob(base64));
      this.tokenExpiration = new Date(this.tokenClaims.exp * 1000);
      this.tokenDuration = (this.tokenExpiration.getTime() - new Date(this.tokenClaims.iat * 1000).getTime()) / 1000;
    }
  }
}
