import config from '../config';
import * as apiUtils from './apiUtils';
import {User} from '../types';

export const currentUser = async (): Promise<User> => {
  const response = await fetch(`${config.apiUrl}/users/current-user`, {
    method: 'GET',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleJsonResponse<User>(response);
};

export const users = async (): Promise<User[]> => {
  const response = await fetch(`${config.apiUrl}/users`, {
    method: 'GET',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleJsonResponse<User[]>(response);
};

export const createUser = async (userRequest: object): Promise<User> => {
  const response = await fetch(`${config.apiUrl}/users`, {
    method: 'POST',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(userRequest)
  });
  return apiUtils.handleJsonResponse<User>(response);
};

export const updateUser = async (id: number, userRequest: object): Promise<User> => {
  const response = await fetch(`${config.apiUrl}/users/${id}`, {
    method: 'PUT',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(userRequest)
  });
  return apiUtils.handleJsonResponse<User>(response);
};

export const deleteUser = async (id: number) => {
  const response = await fetch(`${config.apiUrl}/users/${id}`, {
    method: 'DELETE',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleEmptyResponse(response);
};