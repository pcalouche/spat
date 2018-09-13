import React, {Component}                  from 'react';
import {Card, CardBody, CardHeader, Table} from 'reactstrap';

import './UserList.css';
import {TOKEN}                             from 'util/SecurtyUtils';

class UserList extends Component {
  state = {
    users: []
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

  render() {
    return (
      <Card className="UserList m-2">
        <CardHeader>Users</CardHeader>
        <CardBody>
          <Table striped bordered hover>
            <thead>
              <tr>
                <th>Id</th>
                <th>Username</th>
                <th>Account Status</th>
                <th>Roles</th>
              </tr>
            </thead>
            <tbody>
              {this.state.users.map(user => {
                return (
                  <tr key={user.id}>
                    <td>{user.id}</td>
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

export default UserList;