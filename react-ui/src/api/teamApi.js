import config from '../config';
import * as apiHelper from './apiHelper';

export const teams = async () => {
  const response = await fetch(`${config.apiUrl}/teams`, {
    method: 'GET',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    }
  });
  return apiHelper.handleJsonResponse(response);
};

export const createTeam = async (teamRequest) => {
  const response = await fetch(`${config.apiUrl}/teams`, {
    method: 'POST',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    },
    body: JSON.stringify(teamRequest)
  });
  return apiHelper.handleJsonResponse(response);
};

export const updateTeam = async (id, teamRequest) => {
  const response = await fetch(`${config.apiUrl}/teams/${id}`, {
    method: 'PUT',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    },
    body: JSON.stringify(teamRequest)
  });
  return apiHelper.handleJsonResponse(response);
};

export const deleteTeam = async (id) => {
  const response = await fetch(`${config.apiUrl}/teams/${id}`, {
    method: 'DELETE',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    }
  });
  return apiHelper.handleEmptyResponse(response);
};