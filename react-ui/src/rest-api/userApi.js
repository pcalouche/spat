import {baseUrl, handleEmptyResponse, handleJsonResponse, jwtHeaders} from './apiUtils';

export const fetchUser = async (username) => {
  const response = await fetch(baseUrl + '/users/' + username, {
    headers: jwtHeaders()
  });

  return handleJsonResponse(response);
};

export const fetchCurrentUser = async () => {
  const response = await fetch(baseUrl + '/users/current-user', {
    headers: jwtHeaders()
  });
  return handleJsonResponse(response);
};

export const fetchUsers = async () => {
  const response = await fetch(baseUrl + '/users', {
    headers: jwtHeaders()
  });
  return handleJsonResponse(response);
};

export const addUser = async (user) => {
  const response = await fetch(baseUrl + '/users', {
    method: 'POST',
    headers: jwtHeaders(),
    body: JSON.stringify(user)
  });

  return handleJsonResponse(response);
};

export const editUser = async (user) => {
  const response = await fetch(baseUrl + '/user/' + user.username, {
    method: 'PUT',
    headers: jwtHeaders(),
    body: JSON.stringify(user)
  });
  return handleJsonResponse(response);
};

export const deleteUser = async (user) => {
  const response = await fetch(baseUrl + '/users/' + user.username, {
    method: 'DELETE',
    headers: jwtHeaders()
  });
  return handleEmptyResponse(response);
};