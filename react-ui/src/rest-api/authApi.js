import {baseUrl, basicAuthHeaders, jwtRefreshHeaders} from './apiUtils';

export const fetchToken = async (username, password) => {
  const response = await fetch(baseUrl + '/auth/token', {
    method: 'POST',
    headers: basicAuthHeaders(username, password)
  });
  return response.json();
};

export const refreshToken = async () => {
  const response = await fetch(baseUrl + '/auth/refresh-token', {
    method: 'POST',
    headers: jwtRefreshHeaders()
  });
  return response.json();
};