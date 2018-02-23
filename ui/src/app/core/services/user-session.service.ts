import { Injectable } from '@angular/core';
import { ClientUser } from '@core/model/ClientUser';
import { AuthResponse } from '@rest-services/api/model/auth-response.model';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class UserSessionService {
  private readonly spatSessionKey: string = 'spatSession';
  private readonly tokenKey: string = 'token';
  private readonly refreshTokenKey: string = 'refreshToken';
  private readonly loggedInUserKey: string = 'loggedInUser';
  private tokenClaims: any;
  private tokenExpiration: Date;
  private tokenDuration: number;
  private loggedInUser = new BehaviorSubject<ClientUser>(null);
  private loggedInUserObservable = this.loggedInUser.asObservable();

  constructor() {
    // Check session storage for session on creation
    if (sessionStorage.getItem(this.spatSessionKey)) {
      this.parseClaims(this.getToken());
      const spatSession = JSON.parse(sessionStorage.getItem(this.spatSessionKey));
      this.loggedInUser.next(new ClientUser(JSON.parse(spatSession[this.loggedInUserKey])));
    }
  }

  storeTokens(authResponse: AuthResponse) {
    // Parse the claims
    this.parseClaims(authResponse.token);
    // Store tokens in session storage object
    const spatSession = sessionStorage.getItem(this.spatSessionKey) ? JSON.parse(sessionStorage.getItem(this.spatSessionKey)) : {};
    spatSession[this.tokenKey] = authResponse.token;
    spatSession[this.refreshTokenKey] = authResponse.refreshToken;
    sessionStorage.setItem(this.spatSessionKey, JSON.stringify(spatSession));
  }

  clearSession() {
    this.tokenClaims = null;
    this.tokenExpiration = null;
    this.loggedInUser.next(null);
    sessionStorage.removeItem(this.spatSessionKey);
  }

  getToken(): string {
    return JSON.parse(sessionStorage.getItem(this.spatSessionKey))[this.tokenKey];
  }

  getRefreshToken(): string {
    return JSON.parse(sessionStorage.getItem(this.spatSessionKey))[this.refreshTokenKey];
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

  getLoggedInUser(): ClientUser {
    return this.loggedInUser.getValue();
  }

  setLoggedInUser(loggedInUser: ClientUser) {
    this.loggedInUser.next(loggedInUser);
    const spatSession = JSON.parse(sessionStorage.getItem(this.spatSessionKey));
    spatSession[this.loggedInUserKey] = JSON.stringify(loggedInUser);
    sessionStorage.setItem(this.spatSessionKey, JSON.stringify(spatSession));
  }

  getLoggedInUserAsObservable(): Observable<ClientUser> {
    return this.loggedInUserObservable;
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
