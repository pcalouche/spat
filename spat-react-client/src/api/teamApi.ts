import config from '../config';
import * as apiUtils from './apiUtils';
import {Team} from '../types';

export const teams = async (): Promise<Team[]> => {
  const response = await fetch(`${config.apiUrl}/teams`, {
    method: 'GET',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleJsonResponse<Team[]>(response);
};

export const createTeam = async (teamRequest: object): Promise<Team> => {
  const response = await fetch(`${config.apiUrl}/teams`, {
    method: 'POST',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(teamRequest)
  });
  return apiUtils.handleJsonResponse<Team>(response);
};

export const updateTeam = async (id: number, teamRequest: object): Promise<Team> => {
  const response = await fetch(`${config.apiUrl}/teams/${id}`, {
    method: 'PUT',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(teamRequest)
  });
  return apiUtils.handleJsonResponse<Team>(response);
};

export const deleteTeam = async (id: number) => {
  const response = await fetch(`${config.apiUrl}/teams/${id}`, {
    method: 'DELETE',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleEmptyResponse(response);
};