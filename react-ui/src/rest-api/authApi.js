import {baseUrl, basicAuthHeaders, jwtRefreshHeaders} from './apiUtils';

export const fetchToken = async (username, password) => {
  const response = await fetch(baseUrl + '/auth/token', {
    headers: basicAuthHeaders(username, password)
  });
  return response.json();
};

export const refreshToken = async () => {
  const response = await fetch(baseUrl + '/auth/refresh-token', {
    headers: jwtRefreshHeaders()
  });
  return response.json();
};