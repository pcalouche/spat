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

export const loginFromRefreshToken = async () => {
  const response = await fetch(`${config.apiUrl}/auth/refresh-token`, {
    credentials: 'include',
    method: 'POST'
  });
  const jwt = await apiHelper.handleTextResponse(response);
  localStorage.setItem('token', jwt);
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
    if (window.location.pathname !== '/login') {
      window.location = '/login';
    }
  }
};

export const monitorSession = () => {
  // TODO
  const checkSession = async () => {
    console.info('checking session');
  };
  return setInterval(() => checkSession(), 10 * 1000);
};
