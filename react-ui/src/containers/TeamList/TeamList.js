import React, {Component}                  from 'react';
import {connect}                           from 'react-redux';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';

import './TeamList.css';
import {FontAwesomeIcon}                   from '@fortawesome/react-fontawesome';
import * as teamActions                    from '../../store/actions/teamActions';
import TeamModal                           from '../../components/TeamModal';

class TeamList extends Component {
    state = {
        modalIsOpen: false,
        modalTitle: ''
    };

    openModal = (modalTitle) => {
        this.setState({modalIsOpen: !this.state.modalIsOpen, modalTitle: modalTitle ? modalTitle : ''});
    };

    closeModal = () => {
        this.setState({modalIsOpen: !this.state.modalIsOpen});
    };

    componentDidMount() {
        this.props.loadTeams();
    }

    render() {
        return (
            <Card className="TeamList m-2">
                <CardHeader>Teams</CardHeader>
                <CardBody>
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
                                        <td className="action-column"><FontAwesomeIcon icon="pencil-alt" onClick={() => this.openModal('Edit Team')}/></td>
                                        <td className="action-column"><FontAwesomeIcon icon="trash-alt" onClick={() => this.openModal('Delete Team')}/></td>
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