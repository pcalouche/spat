import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { AuthResponse } from '@rest-services/api/model/auth-response.model';
import { RestServiceHelper } from '@rest-services/api/rest-service-helper';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class AuthService {
  private readonly authRoot = RestServiceHelper.apiRoot + '/auth';
  private readonly tokenUrl: string = this.authRoot + '/token';
  private readonly refreshTokenUrl: string = this.authRoot + '/refresh-token';

  constructor(private http: HttpClient) {
  }

  token(loginForm: FormGroup): Observable<AuthResponse> {
    const authHeaderValue = RestServiceHelper.authHeaderBasicPrefix + btoa(loginForm.value.username + ':' + loginForm.value.password);
    const headers = new HttpHeaders().set(RestServiceHelper.authHttpHeader, authHeaderValue);
    return this.http.get<AuthResponse>(this.tokenUrl, {headers: headers});
  }

  refreshToken(refreshToken): Observable<AuthResponse> {
    const authHeaderValue = RestServiceHelper.authHeaderBearerPrefix + refreshToken;
    const headers = new HttpHeaders().set(RestServiceHelper.authHttpHeader, authHeaderValue);
    return this.http.get<AuthResponse>(this.refreshTokenUrl, {headers: headers});
  }
}
