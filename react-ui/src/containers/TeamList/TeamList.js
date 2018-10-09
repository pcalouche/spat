import React, {Component} from 'react';
import {connect} from 'react-redux';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';

import './TeamList.css';
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';
import * as actionCreators from '../../store/actions/teamActions';

class TeamList extends Component {
    state = {
        // teams: []
    };

    componentDidMount() {
        // fetch('http://localhost:10000/spat/rest-services/api/teams', {
        //   headers: {'Authorization': TOKEN}
        // }).then(results => {
        //   return results.json();
        // }).then(data => {
        //   this.setState({teams: data});
        // }).catch(error => {
        //   console.error(error);
        // });
        this.props.onUsersLoaded();
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
                            {this.props.teams.map(team => {
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

const mapStateToProps = (state) => {
    return {
        teams: state.teams.list,
        selectedTeam: state.teams.selected
    };
};

const mapDispatchToProps = (dispatch) => {
    return {
        onUsersLoaded: () => dispatch(actionCreators.loadTeams())
    };
};

export default connect(mapStateToProps, mapDispatchToProps)(TeamList);