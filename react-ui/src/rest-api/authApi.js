import {baseUrl, basicAuthHeaders} from './apiUtils';

export const fetchToken = async (username, password) => {
  const response = await fetch(baseUrl + '/auth/token', {
    headers: basicAuthHeaders(username, password)
  });
  return response.json();
};