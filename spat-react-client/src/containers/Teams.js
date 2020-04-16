import React, {useCallback, useEffect, useState} from 'react';
import {Button, Card, CardBody, CardHeader, Container, Table} from 'reactstrap';
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';

import {useAppContext} from '../hooks';
import {teamApi} from '../api';
import {ConfirmationModal, TeamModal} from '../components';

const Teams = () => {
  const {isAdmin} = useAppContext();
  const [loadError, setLoadError] = useState(false);
  const [teams, setTeams] = useState(undefined);
  const [selectedTeam, setSelectedTeam] = useState(undefined);
  const [teamModalState, setTeamModalState] = useState({isOpen: false, mode: undefined});
  const [deleteModalIsOpen, setDeleteModalIsOpen] = useState(false);

  const addTeamHandler = () => {
    setSelectedTeam({name: ''});
    setTeamModalState({isOpen: true, mode: 'Add'});
  };

  const editTeamHandler = team => {
    setSelectedTeam(team);
    setTeamModalState({isOpen: true, mode: 'Edit'});
  };

  const deleteTeamHandler = team => {
    setSelectedTeam(team);
    setDeleteModalIsOpen(true);
  };

  const deleteSelectedTeam = async () => {
    try {
      await teamApi.deleteTeam(selectedTeam.id);
      setTeams(prevUsers => prevUsers.filter(team => team.id !== selectedTeam.id));
      setDeleteModalIsOpen(false);
    } catch (error) {
      // Handle cases where it may have been deleted on another tab or someone else and
      // the current screen is stale
      if (error.status === 404) {
        setTeams(prevUsers => prevUsers.filter(user => user.id !== selectedTeam.id));
        setDeleteModalIsOpen(false);
      } else {
        console.error(error);
      }
    }
  };

  const loadTeams = useCallback(
    async () => {
      if (!teams) {
        try {
          const teams = await teamApi.teams();
          setTeams(teams);
        } catch (error) {
          setLoadError(true);
        }
      }
    },
    [teams]
  );

  useEffect(() => {
    loadTeams();
  }, [loadTeams]);

  return (
    <Container fluid className="Users mt-5">
      <Card>
        <CardHeader tag="h5">Teams</CardHeader>
        {loadError ?
          <CardBody>
            <h1>Unable to Load Teams.</h1>
          </CardBody>
          :
          <>
            {isAdmin &&
            <CardBody className="d-flex align-items-center justify-content-end">
              <Button color="primary" onClick={addTeamHandler}>Add Team</Button>
            </CardBody>
            }
            <Table bordered striped hover responsive className="m-0">
              <thead>
                <tr>
                  <th>Name</th>
                  {isAdmin && <th className="text-center">Edit</th>}
                  {isAdmin && <th className="text-center">Delete</th>}
                </tr>
              </thead>
              <tbody>
                {teams && teams.map(team => {
                  return (
                    <tr key={team.id}>
                      <td>{team.name}</td>
                      {isAdmin && <td className="text-center">
                        <Button color="link"
                                title="Edit Team"
                                onClick={() => editTeamHandler(team)}>
                          <FontAwesomeIcon icon="pencil-alt"/>
                        </Button>
                      </td>
                      }
                      {isAdmin && <td className="text-center">
                        <Button color="link"
                                className="text-danger"
                                title="Delete Team"
                                onClick={() => deleteTeamHandler(team)}>
                          <FontAwesomeIcon icon="trash-alt"/>
                        </Button>
                      </td>
                      }
                    </tr>
                  );
                })}
              </tbody>
            </Table>
            {teamModalState.isOpen &&
            <TeamModal isOpen={teamModalState.isOpen}
                       mode={teamModalState.mode}
                       team={selectedTeam}
                       submitCallback={async () => {
                         setTeams(await teamApi.teams());
                         setTeamModalState({isOpen: false, mode: undefined});
                       }}
                       cancelCallback={() => setTeamModalState({isOpen: false, mode: undefined})}>
            </TeamModal>
            }
            {deleteModalIsOpen &&
            <ConfirmationModal isOpen={deleteModalIsOpen}
                               confirmCallback={deleteSelectedTeam}
                               cancelCallback={() => setDeleteModalIsOpen(false)}
                               confirmButtonColor="danger">
              Are you sure you want to delete <span className="text-danger font-weight-bold">{selectedTeam.name}</span>?
            </ConfirmationModal>
            }
          </>
        }
      </Card>
    </Container>
  );
};

export default Teams;