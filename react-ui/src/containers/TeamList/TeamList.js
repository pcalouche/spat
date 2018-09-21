import React, {Component}                  from 'react';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';

import './TeamList.css';
import {TOKEN}                             from '../../util/SecurityUtils';
import {FontAwesomeIcon}                   from '@fortawesome/react-fontawesome';

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
                <th className="action-column"></th>
                <th className="action-column"></th>
                <th>Id</th>
                <th>Name</th>
              </tr>
            </thead>
            <tbody>
              {this.state.teams.map(team => {
                return (
                  <tr key={team.id}>
                    <td className="action-column"><FontAwesomeIcon icon="pencil-alt"></FontAwesomeIcon></td>
                    <td className="action-column"><FontAwesomeIcon icon="trash-alt"></FontAwesomeIcon></td>
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