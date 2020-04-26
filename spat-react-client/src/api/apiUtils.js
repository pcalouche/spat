import * as authApi from './authApi';

export const jsonHeader = {
  'Content-Type': 'application/json;'
};

export const buildBasicAuthHeader = (username, password) => {
  return {
    'Authorization': 'Basic ' + btoa(username + ':' + password)
  };
};

export const buildJwtHeader = () => {
  return {
    'Authorization': 'Bearer ' + localStorage.getItem('token')
  };
};

const updateLastActivity = () => {
  localStorage.setItem('lastActivity', new Date().toISOString());
};

const handleError = async (response) => {
  const contentType = response.headers.get('content-type');
  if (contentType && contentType.indexOf('application/json') !== -1) {
    // throw the json response as an error to get status, code, message, etc.
    const jsonError = await response.json();
    console.error(jsonError);
    // Log user out if unexpected unauthorized error happened
    if (!response.url.includes('/auth/token') &&
      !response.url.includes('/auth/refresh-token') &&
      jsonError.status === 401) {
      await authApi.logout('Authorization error. You are being signed out.');
      window.location = '/login';
    } else {
      throw jsonError;
    }
  } else {
    // throw the response as an error to get status code, message, etc.
    console.error(response);
    throw response;
  }
};

export const handleJsonResponse = async (response) => {
  if (response.ok) {
    // Update last activity on successful server responses
    updateLastActivity();
    return response.json();
  } else {
    await handleError(response);
  }
};

export const handleTextResponse = async response => {
  if (response.ok) {
    // Update last activity on successful server responses, unless it was a refresh token request.
    // This request is considered a background request and not a user initiated one.
    if (!response.url.includes('/auth/refresh-token')) {
      updateLastActivity();
    }
    return response.text();
  } else {
    await handleError(response);
  }
};

export const handleEmptyResponse = async response => {
  if (response.ok) {
    // Update last activity on successful server responses
    updateLastActivity();
  } else {
    await handleError(response);
  }
};