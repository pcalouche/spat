import React, {Component}                  from 'react';
import {connect}                           from 'react-redux';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';

import './TeamList.css';
import {FontAwesomeIcon}                   from '@fortawesome/react-fontawesome';
import * as teamActions                    from '../../redux/actions/team';
import TeamModal                           from '../../components/TeamModal';

class TeamList extends Component {
  state = {
    isLoading: true,
    errorLoading: false,
    modalIsOpen: false,
    modalTitle: ''
  };

  openModal = (modalTitle) => {
    this.setState({modalIsOpen: !this.state.modalIsOpen, modalTitle: modalTitle ? modalTitle : ''});
  };

  closeModal = () => {
    this.setState({modalIsOpen: !this.state.modalIsOpen});
  };

  async componentDidMount() {
    try {
      await this.props.loadTeams();
      this.setState({isLoading: false});
    } catch (error) {
      this.setState({isLoading: false, errorLoading: true});
    }
  }

  render() {
    let content;
    if (this.state.isLoading) {
      content = (<React.Fragment>Loading</React.Fragment>);
    } else if (this.state.errorLoading) {
      content = (<React.Fragment>Error Loading Teams</React.Fragment>);
    } else {
      content = (
        <React.Fragment>
          <Table striped bordered hover>
            <thead>
              <tr>
                <th className="action-column"/>
                <th className="action-column"/>
                <th>Id</th>
                <th>Name</th>
              </tr>
            </thead>
            <tbody>
              {this.props.teams.map(team => {
                return (
                  <tr key={team.id}>
                    <td className="action-column">
                      <FontAwesomeIcon icon="pencil-alt" onClick={() => this.openModal('Edit Team')}/></td>
                    <td className="action-column">
                      <FontAwesomeIcon icon="trash-alt" onClick={() => this.openModal('Delete Team')}/></td>
                    <td>{team.id}</td>
                    <td>{team.name}</td>
                  </tr>
                );
              })}
            </tbody>
          </Table>
          <TeamModal
            open={this.state.modalIsOpen}
            title={this.state.modalTitle}
            cancelModal={this.closeModal}
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
    loggedInUser: state.auth.loggedInUser,
    teams: state.teams.list,
    selectedTeam: state.teams.selected
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    loadTeams: () => dispatch(teamActions.loadTeams())
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(TeamList);