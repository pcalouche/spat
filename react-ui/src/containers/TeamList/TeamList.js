import React, {Component}                  from 'react';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';

import './TeamList.css';
import {TOKEN}                             from '../../util/SecurtyUtils';

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
      <Card className="TeamList m-2">
        <CardHeader>Teams</CardHeader>
        <CardBody>
          <Table striped bordered hover>
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
          </Table>
        </CardBody>
      </Card>
    );
  }
}

export default TeamList;