import { Injectable } from '@angular/core';
import { AuthResponse } from '@app/rest/api/model/auth-response.model';
import { ClientUser } from '@core/model/ClientUser';

@Injectable()
export class SessionManagementService {
  private readonly storageKey: string = 'spatSession';
  private readonly tokenKey: string = 'token';
  private readonly refreshTokenKey: string = 'refreshToken';
  private readonly loggedInUserKey: string = 'loggedInUser';
  private tokenClaims: any;
  private tokenExpiration: Date;
  private tokenDuration: number;
  private loggedInUser: ClientUser = null;

  constructor() {
    // Initialize session storage open if it does not exist
    if (!sessionStorage.getItem(this.storageKey)) {
      const initSession = {};
      initSession[this.tokenKey] = null;
      initSession[this.refreshTokenKey] = null;
      initSession[this.loggedInUserKey] = null;
      sessionStorage.setItem(this.storageKey, JSON.stringify(initSession));
    }
  }

  storeTokens(authResponse: AuthResponse) {
    // Parse the claims
    this.parseClaims(authResponse.token);
    // Store tokens in session storage object
    const spatSession = JSON.parse(sessionStorage.getItem(this.storageKey));
    spatSession[this.tokenKey] = authResponse.token;
    spatSession[this.refreshTokenKey] = authResponse.refreshToken;
    sessionStorage.setItem(this.storageKey, JSON.stringify(spatSession));
    console.log(sessionStorage.getItem(this.storageKey));
  }

  clearSession() {
    this.tokenClaims = null;
    this.tokenExpiration = null;
    this.loggedInUser = null;
    const initSession = {};
    initSession[this.tokenKey] = null;
    initSession[this.refreshTokenKey] = null;
    initSession[this.loggedInUserKey] = null;
    sessionStorage.setItem(this.storageKey, JSON.stringify(initSession));
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

  getLoggedInUser(): ClientUser {
    if (!this.loggedInUser && JSON.parse(sessionStorage.getItem(this.storageKey))[this.loggedInUserKey]) {
      this.loggedInUser = new ClientUser(JSON.parse(sessionStorage.getItem(this.storageKey))[this.loggedInUserKey]);
    }
    return this.loggedInUser;
  }

  setLoggedInUser(loggedInUser: ClientUser) {
    const spatSession = JSON.parse(sessionStorage.getItem(this.storageKey));
    spatSession[this.loggedInUserKey] = JSON.stringify(loggedInUser);
    sessionStorage.setItem(this.storageKey, JSON.stringify(spatSession));
  }

  private parseClaims(token: string) {
    const base64Url = token.split('.')[1];
    const base64 = base64Url.replace('-', '+').replace('_', '/');
    this.tokenClaims = JSON.parse(window.atob(base64));
    this.tokenExpiration = new Date(this.tokenClaims.exp * 1000);
    this.tokenDuration = (this.tokenExpiration.getTime() - new Date(this.tokenClaims.iat * 1000).getTime()) / 1000;
  }
}
