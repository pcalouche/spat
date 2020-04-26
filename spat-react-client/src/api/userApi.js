import config from '../config';
import * as apiUtils from './apiUtils';

export const currentUser = async () => {
  const response = await fetch(`${config.apiUrl}/users/current-user`, {
    method: 'GET',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleJsonResponse(response);
};

export const users = async () => {
  const response = await fetch(`${config.apiUrl}/users`, {
    method: 'GET',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleJsonResponse(response);
};

export const createUser = async (userRequest) => {
  const response = await fetch(`${config.apiUrl}/users`, {
    method: 'POST',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(userRequest)
  });
  return apiUtils.handleJsonResponse(response);
};

export const updateUser = async (id, userRequest) => {
  const response = await fetch(`${config.apiUrl}/users/${id}`, {
    method: 'PUT',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    },
    body: JSON.stringify(userRequest)
  });
  return apiUtils.handleJsonResponse(response);
};

export const deleteUser = async (id) => {
  const response = await fetch(`${config.apiUrl}/users/${id}`, {
    method: 'DELETE',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildJwtHeader()
    }
  });
  return apiUtils.handleEmptyResponse(response);
};