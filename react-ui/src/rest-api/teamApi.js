import {baseUrl, handleJsonResponse, jwtHeaders} from './apiUtils';

export const fetchTeams = async () => {
  const response = await fetch(baseUrl + '/teams', {
    headers: jwtHeaders()
  });
  return handleJsonResponse(response);
};

export const addTeam = async (team) => {
  const response = await fetch(baseUrl + '/teams', {
    method: 'POST',
    body: JSON.stringify(team),
    headers: jwtHeaders()
  });
  return handleJsonResponse(response);
};

export const editTeam = async (team) => {
  const response = await fetch(baseUrl + '/teams/' + team.id, {
    method: 'UPDATE',
    body: JSON.stringify(team),
    headers: jwtHeaders()
  });
  return handleJsonResponse(response);
};

export const deleteTeam = async (team) => {
  return await fetch(baseUrl + '/teams/' + team.id, {
    method: 'DELETE',
    headers: jwtHeaders()
  });
};