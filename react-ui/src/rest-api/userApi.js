import {baseUrl, handleJsonResponse, jwtHeaders} from './apiUtils';

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