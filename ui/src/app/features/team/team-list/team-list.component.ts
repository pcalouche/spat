import { Component, OnInit } from '@angular/core';
import { Team } from '@rest-services/api/model/team.model.a';
import { TeamService } from '@rest-services/api/team/team.service';

@Component({
  selector: 'app-team-list',
  templateUrl: './team-list.component.html',
  styleUrls: ['./team-list.component.scss']
})
export class TeamListComponent implements OnInit {
  teams: Team[] = [];

  constructor(private teamService: TeamService) {
  }

  ngOnInit() {
    this.teamService.teams().subscribe(
      teams => {
        this.teams = teams;
      }
    );
  }

}
