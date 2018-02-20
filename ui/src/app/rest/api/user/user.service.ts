import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { User } from '@app/rest/api/model/user.model';
import { RestServiceHelper } from '@app/rest/api/rest-service-helper';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class UserService {
  private readonly userRoot = RestServiceHelper.apiRoot + '/user';

  constructor(private http: HttpClient) {
  }

  getByUsername(username: string): Observable<User> {
    return this.http.get<User>(this.userRoot + '/' + username);
  }
}
