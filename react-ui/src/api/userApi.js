import config from '../config';
import * as apiHelper from './apiHelper';

export const currentUser = async () => {
  const response = await fetch(`${config.apiUrl}/users/current-user`, {
    method: 'GET',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    }
  });
  return apiHelper.handleJsonResponse(response);
};

export const users = async () => {
  const response = await fetch(`${config.apiUrl}/users`, {
    method: 'GET',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    }
  });
  return apiHelper.handleJsonResponse(response);
};

export const createUser = async (userRequest) => {
  const response = await fetch(`${config.apiUrl}/users`, {
    method: 'POST',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    },
    body: JSON.stringify(userRequest)
  });
  return apiHelper.handleJsonResponse(response);
};

export const updateUser = async (id, userRequest) => {
  const response = await fetch(`${config.apiUrl}/users/${id}`, {
    method: 'PUT',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    },
    body: JSON.stringify(userRequest)
  });
  return apiHelper.handleJsonResponse(response);
};

export const deleteUser = async (id) => {
  const response = await fetch(`${config.apiUrl}/users/${id}`, {
    method: 'DELETE',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.jwtHeader()
    }
  });
  return apiHelper.handleEmptyResponse(response);
};