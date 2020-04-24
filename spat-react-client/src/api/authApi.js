import config from '../config';
import * as apiHelper from './apiHelper';

export const login = async ({username, password}) => {
  const response = await fetch(`${config.apiUrl}/auth/token`, {
    credentials: 'include',
    method: 'POST',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.basicAuthHeader(username, password)
    }
  });
  return apiHelper.handleTextResponse(response);
};

export const logout = async () => {
  try {
    await fetch(`${config.apiUrl}/auth/token`, {
      credentials: 'include',
      method: 'DELETE'
    });
  } catch (error) {
    // do nothing if we can't communicate with server during logout
  } finally {
    localStorage.clear();
    window.location = '/login';
  }
};
