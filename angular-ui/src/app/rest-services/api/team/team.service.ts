import {HttpClient}        from '@angular/common/http';
import {Injectable}        from '@angular/core';
import {Team}              from '@rest-services/api/model/team.model.a';
import {RestServiceHelper} from '@rest-services/api/rest-service-helper';
import {Observable}        from 'rxjs';

@Injectable()
export class TeamService {
  private readonly teamRoot = RestServiceHelper.apiRoot + '/teams';

  constructor(private http: HttpClient) {
  }

  teams(): Observable<Team[]> {
    return this.http.get<Team[]>(this.teamRoot);
  }

  saveTeam(team: Team): Observable<Team> {
    return this.http.post<Team>(this.teamRoot, team);
  }

  deleteTeam(team: Team): Observable<boolean> {
    return this.http.delete<boolean>(this.teamRoot + '/' + team.id);
  }
}
