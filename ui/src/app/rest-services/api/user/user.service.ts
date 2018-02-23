import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { User } from '@rest-services/api/model/user.model';
import { RestServiceHelper } from '@rest-services/api/rest-service-helper';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class UserService {
  private readonly userRoot = RestServiceHelper.apiRoot + '/user';

  constructor(private http: HttpClient) {
  }

  users(): Observable<User[]> {
    return this.http.get<User[]>(this.userRoot);
  }

  getByUsername(username: string): Observable<User> {
    return this.http.get<User>(this.userRoot + '/' + username);
  }
}
