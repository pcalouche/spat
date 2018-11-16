import React, {Component}                          from 'react';
import {connect}                                   from 'react-redux';
import {Button, Card, CardBody, CardHeader, Table} from 'reactstrap';

import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';
import * as teamActions  from '../../redux/actions/team';
import TeamModal         from '../../components/TeamModal';

class TeamList extends Component {

  componentDidMount = () => {
    this.props.loadTeams();
  };

  render() {
    let content;
    if (this.props.loading) {
      content = (<h1>Loading...</h1>);
    } else if (this.props.showError) {
      content = (<h1>Error Loading Teams</h1>);
    } else {
      content = (
        <React.Fragment>
          {this.props.isAdmin &&
          <Button
            color="secondary"
            className="mb-2"
            onClick={() => this.props.showModal('Add', {name: ''}, this.props.addTeam)}>
            Add Team
          </Button>
          }
          <Table striped bordered hover>
            <thead>
              <tr>
                {this.props.isAdmin && <th className="action-column"/>}
                {this.props.isAdmin && <th className="action-column"/>}
                <th>Id</th>
                <th>Name</th>
              </tr>
            </thead>
            <tbody>
              {this.props.teams.map(team => {
                return (
                  <tr key={team.id}>
                    {this.props.isAdmin &&
                    <td className="action-column">
                      <FontAwesomeIcon
                        icon="pencil-alt"
                        onClick={() => this.props.showModal('Edit', team, this.props.editTeam)}/>
                    </td>
                    }
                    {this.props.isAdmin &&
                    <td className="action-column">
                      <FontAwesomeIcon
                        icon="trash-alt"
                        onClick={() => this.props.showModal('Delete', team, this.props.deleteTeam)}/>
                    </td>
                    }
                    <td>{team.id}</td>
                    <td>{team.name}</td>
                  </tr>
                );
              })}
            </tbody>
          </Table>
          <TeamModal
            open={this.props.modalIsOpen}
            mode={this.props.modalMode}
            team={this.props.selectedTeam}
            errorMessage={this.props.modalError}
            submitCallback={this.props.modalSubmitCallback}
            cancelCallback={this.props.hideModal}
          />
        </React.Fragment>
      );
    }

    return (
      <Card className="TeamList m-2">
        <CardHeader>Teams</CardHeader>
        <CardBody>
          {content}
        </CardBody>
      </Card>
    );
  }
}

const mapStateToProps = (state) => {
  return {
    isAdmin: state.auth.tokenClaims && state.auth.tokenClaims.authorities.indexOf('ROLE_ADMIN') !== -1,
    loading: state.teams.loading,
    showError: state.teams.showError,
    teams: state.teams.list,
    selectedTeam: state.teams.selectedTeam,
    modalIsOpen: state.teams.modalIsOpen,
    modalMode: state.teams.modalMode,
    modalError: state.teams.modalError,
    modalSubmitCallback: state.teams.modalSubmitCallback
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    loadTeams: () => dispatch(teamActions.loadTeams()),
    showModal: (mode, team, submitCallback) => dispatch(teamActions.showTeamModal(mode, team, submitCallback)),
    hideModal: () => dispatch(teamActions.hideTeamModal()),
    addTeam: (team) => dispatch(teamActions.addTeam(team)),
    editTeam: (team) => dispatch(teamActions.editTeam(team)),
    deleteTeam: (team) => dispatch(teamActions.deleteTeam(team))
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(TeamList);