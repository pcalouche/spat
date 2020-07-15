import config from '../config';
import * as apiUtils from './apiUtils';

export const login = async (username: string, password: string) => {
  const response = await fetch(`${config.apiUrl}/auth/token`, {
    credentials: 'include',
    method: 'POST',
    headers: {
      ...apiUtils.jsonHeader,
      ...apiUtils.buildBasicAuthHeader(username, password)
    }
  });
  return apiUtils.handleTextResponse(response);
};

export const requestNewToken = async () => {
  const response = await fetch(`${config.apiUrl}/auth/refresh-token`, {
    credentials: 'include',
    method: 'POST'
  });
  const jwt = await apiUtils.handleTextResponse(response);
  localStorage.setItem('token', jwt);
};

export const logout = async (message?: string) => {
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
      window.location.href = '/login?signOut=true';
    }
    if (message) {
      alert(message);
    }
  }
};

export const monitorSession = () => {
  // Update if server configuration changes this
  const tokenDuration = 15 * 60 * 1000;
  const checkSession = async () => {
    if (localStorage.getItem('token')) {
      const lastActivityFromLocalStorage = localStorage.getItem('lastActivity');
      const lastActivity = typeof lastActivityFromLocalStorage === 'string' ? new Date(lastActivityFromLocalStorage) : new Date();
      console.debug('lastActivity', lastActivity);
      const timeElapsed = new Date().getTime() - lastActivity.getTime();
      console.debug('timeElapsed', timeElapsed);
      const timeLeft = tokenDuration - timeElapsed;
      console.debug('timeLeft', timeLeft);
      if (timeLeft <= 0) {
        await logout('You have been logged out from inactivity.');
      } else if (timeLeft <= 2 * 60 * 1000) {
        // If less than two minutes left, start trying to refresh the token in the background
        try {
          console.debug('attempting to acquire new token in the background');
          await requestNewToken();
        } catch (error) {
          if (error instanceof TypeError) {
            console.debug('Server response not received. Failing silently in hopes this was just a network hiccup.', error);
          } else {
            console.debug('Error happened during token refresh, so signing out.');
            await logout('Authorization error. You are being signed out.');
          }
        }
      }
    }
  };
  // Check every minute
  return setInterval(() => checkSession(), 60 * 1000);
};
