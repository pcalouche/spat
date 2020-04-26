import config from '../config';
import * as apiUtils from './apiUtils';

export const teams = async () => {
  const response = await fetch(`${config.apiUrl}/teams`, {
    method: 'GET',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleJsonResponse(response);
};

export const createTeam = async (teamRequest) => {
  const response = await fetch(`${config.apiUrl}/teams`, {
    method: 'POST',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(teamRequest)
  });
  return apiUtils.handleJsonResponse(response);
};

export const updateTeam = async (id, teamRequest) => {
  const response = await fetch(`${config.apiUrl}/teams/${id}`, {
    method: 'PUT',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(teamRequest)
  });
  return apiUtils.handleJsonResponse(response);
};

export const deleteTeam = async (id) => {
  const response = await fetch(`${config.apiUrl}/teams/${id}`, {
    method: 'DELETE',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleEmptyResponse(response);
};