import React, {useEffect, useState} from 'react';
import {Button, Card, Container, Table} from 'react-bootstrap';
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome';

import {useAppContext} from '../hooks';
import {teamApi} from '../api';
import {Team} from '../types';
import {ConfirmationModal, TeamModal} from '../components';

const Teams: React.FC = () => {
  const {isAdmin} = useAppContext();
  const [loadError, setLoadError] = useState(false);
  const [teams, setTeams] = useState<Team[] | undefined>(undefined);
  const [selectedTeam, setSelectedTeam] = useState<Team>({id: -1, name: ''});
  const [teamModalState, setTeamModalState] = useState<{isOpen: boolean, mode: 'Add' | 'Edit'}>({
    isOpen: false,
    mode: 'Add'
  });
  const [deleteModalIsOpen, setDeleteModalIsOpen] = useState(false);

  const addTeamHandler = () => {
    setSelectedTeam({id: -1, name: ''});
    setTeamModalState({isOpen: true, mode: 'Add'});
  };

  const editTeamHandler = (team: Team) => {
    setSelectedTeam(team);
    setTeamModalState({isOpen: true, mode: 'Edit'});
  };

  const deleteTeamHandler = (team: Team) => {
    setSelectedTeam(team);
    setDeleteModalIsOpen(true);
  };

  const deleteSelectedTeam = async () => {
    try {
      await teamApi.deleteTeam(selectedTeam.id);
      setTeams(prevTeams => prevTeams ? prevTeams.filter(team => team.id !== selectedTeam.id) : []);
      setDeleteModalIsOpen(false);
    } catch (error) {
      // Handle cases where it may have been deleted on another tab or someone else and
      // the current screen is stale
      if (error.status === 404) {
        setTeams(prevTeams => prevTeams ? prevTeams.filter(team => team.id !== selectedTeam.id) : []);
        setDeleteModalIsOpen(false);
      } else {
        console.error(error);
      }
    }
  };

  useEffect(() => {
    const fetchData = async () => {
      if (!teams) {
        try {
          const teams = await teamApi.teams();
          setTeams(teams);
        } catch (error) {
          setLoadError(true);
        }
      }
    };
    fetchData().then();
  }, [teams]);

  return (
    <Container fluid className="Teams mt-5">
      <Card>
        <Card.Header as="h5">Teams</Card.Header>
        {loadError ?
          <Card.Body>
            <h1>Unable to Load Teams.</h1>
          </Card.Body>
          :
          <>
            {isAdmin &&
            <Card.Body className="d-flex align-items-center justify-content-end">
              <Button variant="primary" onClick={addTeamHandler}>Add Team</Button>
            </Card.Body>
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
                      {isAdmin &&
                      <td className="text-center">
                        <Button variant="link"
                                title="Edit Team"
                                onClick={() => editTeamHandler(team)}>
                          <FontAwesomeIcon icon="pencil-alt"/>
                        </Button>
                      </td>
                      }
                      {isAdmin &&
                      <td className="text-center">
                        <Button variant="link"
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
            <TeamModal show={teamModalState.isOpen}
                       mode={teamModalState.mode}
                       team={selectedTeam}
                       submitCallback={async () => {
                         setTeams(await teamApi.teams());
                         setTeamModalState({isOpen: false, mode: 'Add'});
                       }}
                       cancelCallback={() => setTeamModalState({isOpen: false, mode: 'Add'})}>
            </TeamModal>
            <ConfirmationModal show={deleteModalIsOpen}
                               title="Delete Team"
                               confirmCallback={deleteSelectedTeam}
                               cancelCallback={() => setDeleteModalIsOpen(false)}>
              Are you sure you want to delete <span className="text-danger font-weight-bold">{selectedTeam?.name}</span>?
            </ConfirmationModal>
          </>
        }
      </Card>
    </Container>
  );
};

export default Teams;