import React, {useEffect, useState} from 'react';
import {Button, Card, CardBody, CardHeader, Container, Table} from 'reactstrap';
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';

import {useAppContext} from '../hooks';
import {userApi} from '../api';
import {User} from '../types';
import {ConfirmationModal, UserModal} from '../components';

const Users = () => {
  const {isAdmin} = useAppContext();
  const [loadError, setLoadError] = useState(false);
  const [users, setUsers] = useState<User[] | undefined>(undefined);
  const [selectedUser, setSelectedUser] = useState<User>({
    id: -1,
    username: '',
    roles: [],
    accountNonExpired: false,
    accountNonLocked: false,
    credentialsNonExpired: false,
    enabled: false
  });
  const [userModalState, setUserModalState] = useState<{isOpen: boolean, mode: 'Add' | 'Edit'}>({
    isOpen: false,
    mode: 'Add'
  });
  const [deleteModalIsOpen, setDeleteModalIsOpen] = useState(false);

  const getAccountStatusDisplay = (user: User) => {
    const accountStatus = [];
    if (user.enabled) {
      accountStatus.push('Enabled');
    } else {
      accountStatus.push('Disabled');
    }
    if (!user.accountNonLocked) {
      accountStatus.push('Locked');
    }
    if (!user.credentialsNonExpired) {
      accountStatus.push('Credentials Expired');
    }
    if (!user.accountNonExpired) {
      accountStatus.push('Expired');
    }
    return accountStatus.join(', ');
  };

  const addUserHandler = () => {
    setSelectedUser({
      id: -1,
      username: '',
      roles: [],
      enabled: true,
      accountNonLocked: true,
      credentialsNonExpired: true,
      accountNonExpired: true
    });
    setUserModalState({isOpen: true, mode: 'Add'});
  };

  const editUserHandler = (user: User) => {
    setSelectedUser(user);
    setUserModalState({isOpen: true, mode: 'Edit'});
  };

  const deleteUserHandler = (user: User) => {
    setSelectedUser(user);
    setDeleteModalIsOpen(true);
  };

  const deleteSelectedUser = async () => {
    try {
      await userApi.deleteUser(selectedUser.id);
      setUsers(prevUsers => prevUsers ? prevUsers.filter(user => user.id !== selectedUser.id) : []);
      setDeleteModalIsOpen(false);
    } catch (error) {
      // Handle cases where it may have been deleted on another tab or someone else and
      // the current screen is stale
      if (error.status === 404) {
        setUsers(prevUsers => prevUsers ? prevUsers.filter(user => user.id !== selectedUser.id) : []);
        setDeleteModalIsOpen(false);
      } else {
        console.error(error);
      }
    }
  };

  useEffect(() => {
    const fetchData = async () => {
      if (!users) {
        try {
          const users = await userApi.users();
          setUsers(users);
        } catch (error) {
          setLoadError(true);
        }
      }
    };
    fetchData().then();
  }, [users]);

  return (
    <Container fluid className="Users mt-5">
      <Card>
        <CardHeader tag="h5">Users</CardHeader>
        {loadError ?
          <CardBody>
            <h1>Unable to Load Users.</h1>
          </CardBody>
          :
          <>
            {isAdmin &&
            <CardBody className="d-flex align-items-center justify-content-end">
              <Button color="primary" onClick={addUserHandler}> Add User </Button>
            </CardBody>
            }
            <Table bordered striped hover responsive className="m-0">
              <thead>
                <tr>
                  <th>Username</th>
                  <th>Account Status</th>
                  <th>Roles</th>
                  {isAdmin && <th className="text-center">Edit</th>}
                  {isAdmin && <th className="text-center">Delete</th>}
                </tr>
              </thead>
              <tbody>
                {users && users.map(user => {
                  return (
                    <tr key={user.id}>
                      <td>{user.username}</td>
                      <td>{getAccountStatusDisplay(user)}</td>
                      <td>{user.roles.map(item => item.name).join(', ')}</td>
                      {isAdmin && <td className="text-center">
                        <Button color="link"
                                title="Edit User"
                                onClick={() => editUserHandler(user)}>
                          <FontAwesomeIcon icon="pencil-alt"/>
                        </Button>
                      </td>
                      }
                      {isAdmin && <td className="text-center">
                        <Button color="link"
                                className="text-danger"
                                title="Delete User"
                                onClick={() => deleteUserHandler(user)}>
                          <FontAwesomeIcon icon="trash-alt"/>
                        </Button>
                      </td>
                      }
                    </tr>
                  );
                })}
              </tbody>
            </Table>
            {userModalState.isOpen &&
            <UserModal isOpen={userModalState.isOpen}
                       mode={userModalState.mode}
                       user={selectedUser}
                       submitCallback={async () => {
                         setUsers(await userApi.users());
                         setUserModalState({isOpen: false, mode: 'Add'});
                       }}
                       cancelCallback={() => setUserModalState({isOpen: false, mode: 'Add'})}>
            </UserModal>
            }
            {deleteModalIsOpen &&
            <ConfirmationModal isOpen={deleteModalIsOpen}
                               confirmCallback={deleteSelectedUser}
                               cancelCallback={() => setDeleteModalIsOpen(false)}
                               confirmButtonColor="danger">
              Are you sure you want to delete <span className="text-danger font-weight-bold">{selectedUser.username}</span>?
            </ConfirmationModal>
            }
          </>
        }
      </Card>
    </Container>
  );
};

export default Users;