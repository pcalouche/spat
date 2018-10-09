import React, {Component} from 'react';
import {connect} from 'react-redux';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';

import './UserList.css';
import {TOKEN} from '../../util/SecurityUtils';
import * as userActionTypes from '../../store/actions/userActions';

class UserList extends Component {
    state = {
        users: []
    };
    displayAccountStatus = (user) => {
        const accountStatus = [];
        if (user.enabled) {
            accountStatus.push('Enabled');
        } else {
            accountStatus.push('Disabled');
        }
        if (!user.accountNonExpired) {
            accountStatus.push('Expired');
        }
        if (!user.accountNonLocked) {
            accountStatus.push('Locked');
        }
        if (!user.credentialsNonExpired) {
            accountStatus.push('Credentials Expired');
        }
        return accountStatus.join(', ');
    };
    displayRoles = (user) => {
        return user.roles.map(item => item.name).join(', ');
    };

    componentDidMount() {
        fetch('http://localhost:10000/spat/rest-services/api/users', {
            headers: {'Authorization': TOKEN}
        }).then(results => {
            return results.json();
        }).then(data => {
            this.setState({users: data});
        }).catch(error => {
            console.error(error);
        });
    }

    render() {
        return (
            <Card className="UserList m-2">
                <CardHeader>Users</CardHeader>
                <CardBody>
                    <Table striped bordered hover>
                        <thead>
                            <tr>
                                <th className="action-column"></th>
                                <th className="action-column"></th>
                                <th>Username</th>
                                <th>Account Status</th>
                                <th>Roles</th>
                            </tr>
                        </thead>
                        <tbody>
                            {this.state.users.map(user => {
                                return (
                                    <tr key={user.username}>
                                        <td className="action-column"><FontAwesomeIcon icon="pencil-alt"></FontAwesomeIcon></td>
                                        <td className="action-column"><FontAwesomeIcon icon="trash-alt"></FontAwesomeIcon></td>
                                        <td>{user.username}</td>
                                        <td>{this.displayAccountStatus(user)}</td>
                                        <td>{this.displayRoles(user)}</td>
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
        users: state.users.list,
        selectedUser: state.users.selected
    };
};

const mapDispatchToProps = (dispatch) => {
    return {
        onUsersLoaded: (users) => dispatch({type: userActionTypes.LOAD_USERS, list: users})
    };
};

export default connect(mapStateToProps, mapDispatchToProps)(UserList);