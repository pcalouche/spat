import React, {Component} from 'react';
import {connect} from 'react-redux';
import {Button, Card, CardBody, CardHeader, Table} from 'reactstrap';
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';

import * as userActions from '../../redux/actions/user';
import UserModal from '../../components/UserModal';

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

  componentDidMount = () => {
    this.props.loadUsers();
  };

  render() {
    let content;
    if (this.props.loading) {
      content = (<h1>Loading...</h1>);
    } else if (this.props.showError) {
      content = (<h1>Error Loading Users</h1>);
    } else {
      content = (
        <React.Fragment>
          {this.props.isAdmin && <Button
            color="secondary"
            className="mb-2"
            onClick={this.props.showModal('Add', {username: ''}, this.props.addUser)}>
            Add User
          </Button>
          }
          <Table striped bordered hover>
            <thead>
              <tr>
                {this.props.isAdmin && <th className="action-column"/>}
                {this.props.isAdmin && <th className="action-column"/>}
                <th>Username</th>
                <th>Account Status</th>
                <th>Roles</th>
              </tr>
            </thead>
            <tbody>
              {this.props.users.map(user => {
                return (
                  <tr key={user.username}>
                    {this.props.isAdmin &&
                    <td className="action-column">
                      <FontAwesomeIcon
                        icon="pencil-alt"
                        onClick={this.props.showModal('Edit', user, this.props.editUser)}/>
                    </td>
                    }
                    {this.props.isAdmin &&
                    <td className="action-column">
                      <FontAwesomeIcon
                        icon="trash-alt"
                        onClick={this.props.showModal('Delete', user, this.props.deleteUser)}/>
                    </td>
                    }
                    <td>{user.username}</td>
                    <td>{this.displayAccountStatus(user)}</td>
                    <td>{this.displayRoles(user)}</td>
                  </tr>
                );
              })}
            </tbody>
          </Table>
          <UserModal
            open={this.props.modalIsOpen}
            mode={this.props.modalMode}
            user={this.props.selectedUser}
            errorMessage={this.props.modalError}
            submitCallback={this.props.modalSubmitCallback}
            cancelCallback={this.props.hideModal}
          />
        </React.Fragment>
      );
    }

    return (
      <Card className="UserList m-2">
        <CardHeader>Users</CardHeader>
        <CardBody>
          {content}
        </CardBody>
      </Card>
    );
  }
}

const mapStateToProps = (state) => {
  return {
    isAdmin: state.auth.tokenClaims && state.auth.tokenClaims.authorities.indexOf('Admin') !== -1,
    loading: state.users.loading,
    showError: state.users.showError,
    users: state.users.list,
    selectedUser: state.users.selectedUser,
    modalIsOpen: state.users.modalIsOpen,
    modalMode: state.users.modalMode,
    modalError: state.users.modalError,
    modalSubmitCallback: state.users.modalSubmitCallback
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    loadUsers: () => dispatch(userActions.loadUsers()),
    showModal: (mode, team, submitCallback) => () => dispatch(userActions.showUserModal(mode, team, submitCallback)),
    hideModal: () => dispatch(userActions.hideUserModal()),
    addUser: (user) => dispatch(userActions.addUser(user)),
    editUser: (user) => dispatch(userActions.editUser(user)),
    deleteUser: (user) => dispatch(userActions.deleteUser(user))
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(UserList);