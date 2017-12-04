import {Injectable} from '@angular/core';
import {AuthResponse} from '../../rest/model/auth-response.model';

@Injectable()
export class TokenService {
  public static readonly AUTH_HTTP_HEADER: string = 'Authorization';
  public static readonly AUTH_HEADER_VALUE_PREFIX: string = 'Bearer ';
  private readonly storageKey: string = 'trsAuth';
  private readonly tokenKey: string = 'token';
  private readonly refreshTokenKey: string = 'refreshToken';
  private tokenClaims: any;
  private tokenExpiration: Date;
  private tokenDuration: number;

  constructor() {
  }

  clearTokens() {
    sessionStorage.removeItem(this.storageKey);
    this.tokenClaims = null;
    this.tokenExpiration = null;
  }

  storeTokens(authResponse: AuthResponse) {
    let authInfo = {};
    authInfo[this.tokenKey] = authResponse.token;
    authInfo[this.refreshTokenKey] = authResponse.refreshToken;
    sessionStorage.setItem(this.storageKey, JSON.stringify(authInfo));
    this.parseClaims(authResponse.token);
  }

  getToken(): string {
    if (sessionStorage.getItem(this.storageKey)) {
      let token = JSON.parse(sessionStorage.getItem(this.storageKey))[this.tokenKey];
      // If page was refreshed token wil still be in the browser's session storage,
      // but not in the service. The route guard service will call getToken every time,
      // so this will get re-evaluated.
      if (!this.tokenClaims) {
        this.parseClaims(token);
      }
      return token;
    } else {
      return null;
    }
  }

  getRefreshToken(): string {
    if (sessionStorage.getItem(this.storageKey)) {
      return JSON.parse(sessionStorage.getItem(this.storageKey))[this.refreshTokenKey];
    } else {
      return null;
    }
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

  getHeaderValue(): string {
    return TokenService.AUTH_HEADER_VALUE_PREFIX + this.getToken();
  }

  private parseClaims(token: string) {
    let base64Url = token.split('.')[1];
    let base64 = base64Url.replace('-', '+').replace('_', '/');
    this.tokenClaims = JSON.parse(window.atob(base64));
    this.tokenExpiration = new Date(this.tokenClaims.exp * 1000);
    this.tokenDuration = (this.tokenExpiration.getTime() - new Date(this.tokenClaims.iat * 1000).getTime()) / 1000;
  }
}
