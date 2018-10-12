import React, {Component}                  from 'react';
import {connect}                           from 'react-redux';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';
import {FontAwesomeIcon}                   from '@fortawesome/react-fontawesome';

import './UserList.css';
import * as userActions                    from '../../store/actions/userActions';

class UserList extends Component {

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
        this.props.loadUsers();
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
                            {this.props.users.map(user => {
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
        loggedInUser: state.auth.loggedInUser,
        users: state.users.list,
        selectedUser: state.users.selected
    };
};

const mapDispatchToProps = (dispatch) => {
    return {
        loadUsers: () => dispatch(userActions.loadUsers())
    };
};

export default connect(mapStateToProps, mapDispatchToProps)(UserList);