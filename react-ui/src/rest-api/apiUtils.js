export const baseUrl = 'http://localhost:10000/spat/rest-services/api';

const storageKey = 'spatTokenData';

export const jwtHeaders = () => {
  return {
    'Authorization': 'Bearer ' + getToken(),
    'Content-Type': 'application/json;'
  };
};

export const jwtRefreshHeaders = () => {
  return {
    'Authorization': 'Bearer ' + getRefreshToken(),
    'Content-Type': 'application/json;'
  };
};

export const basicAuthHeaders = (username, password) => {
  return {
    'Authorization': 'Basic ' + btoa(username + ':' + password),
    'Content-Type': 'application/json;'
  };
};

export const handleEmptyResponse = async response => {
  if (!response.ok) {
    await logHttpResponseError(response);
    throw new Error('HTTP response error');
  }
};

export const handleJsonResponse = async response => {
  if (!response.ok) {
    await logHttpResponseError(response);
    throw new Error('HTTP response error');
  }
  return response.json();
};

export const logError = (error) => {
  // Don't re-log HTTP response errors
  if (error.message !== 'HTTP response error') {
    console.error(error);
  }
};

export const getTokenData = () => {
  let tokenData = null;
  if (sessionStorage.getItem(storageKey)) {
    tokenData = JSON.parse(sessionStorage.getItem(storageKey));
  }
  return tokenData;
};

export const getToken = () => {
  let token = null;
  if (sessionStorage.getItem(storageKey)) {
    token = JSON.parse(sessionStorage.getItem(storageKey))['token'];
  }
  return token;
};

export const setToken = (tokenData) => {
  sessionStorage.setItem(storageKey, JSON.stringify(tokenData));
};

const getRefreshToken = () => {
  let refreshToken = null;
  if (sessionStorage.getItem(storageKey)) {
    refreshToken = JSON.parse(sessionStorage.getItem(storageKey))['refreshToken'];
  }
  return refreshToken;
};

export const getClaims = () => {
  const claims = {};
  const token = getToken();
  if (token) {
    const base64Url = token.split('.')[1];
    const base64 = base64Url.replace('-', '+').replace('_', '/');
    return JSON.parse(atob(base64));
  }
  return claims;
};

export const clearToken = () => {
  sessionStorage.removeItem(storageKey);
};

const logHttpResponseError = async response => {
  const contentType = response.headers.get('content-type');
  if (contentType && contentType.indexOf('application/json') !== -1) {
    try {
      const json = await response.json();
      console.error(`status: ${json.status} message: ${json.message}`);
      if (json.status === 401 && json.message.indexOf('JWT expired') !== -1) {
        clearToken();
      }
    } catch (error) {
      console.error(`status: ${response.status} message: ${response.message}`);
    }
  } else {
    console.error(`status: ${response.status} message: ${response.message}`);
  }
};