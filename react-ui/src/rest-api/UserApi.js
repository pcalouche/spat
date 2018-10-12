import {baseUrl, handleJsonResponse, jsonHeaders} from './Api';

export const fetchUser = async (username) => {
  const response = await fetch(baseUrl + '/users/' + username, {
    headers: jsonHeaders()
  });

  return handleJsonResponse(response);
};

export const fetchCurrentUser = async () => {
  const response = await fetch(baseUrl + '/users/current-user', {
    headers: jsonHeaders()
  });

  return handleJsonResponse(response);
};

export const fetchUsers = async () => {
  const response = await fetch(baseUrl + '/users', {
    headers: jsonHeaders()
  });
  return handleJsonResponse(response);
};