import React, {Component} from 'react';

import './TeamList.css';
import {TOKEN}            from '../../util/SecurtyUtils';

class TeamList extends Component {
  state = {
    teams: []
  };

  componentDidMount() {
    fetch('http://localhost:10000/spat/rest-services/api/teams', {
      headers: {'Authorization': TOKEN}
    }).then(results => {
      return results.json();
    }).then(data => {
      this.setState({teams: data});
    }).catch(error => {
      console.error(error);
    });
  }

  render() {
    return (
      <div className="TeamList">
        This is the Team List container {this.state.teams.length}
        <table>
          <thead>
            <tr>
              <th>Id</th>
              <th>Name</th>
            </tr>
          </thead>
          <tbody>
            {this.state.teams.map(team => {
              return (
                <tr key={team.id}>
                  <td>{team.id}</td>
                  <td>{team.name}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    );
  }
}

export default TeamList;