export const baseUrl = 'http://localhost:10000/spat/rest-services/api';

export const storageKey = 'spatTokenData';

export const jwtHeaders = () => {
  return {
    'Authorization': 'Bearer ' + getToken(),
    'Content-Type': 'application/json;'
  };
};

export const basicAuthHeaders = (username, password) => {
  return {
    'Authorization': 'Basic ' + btoa(username + ':' + password),
    'Content-Type': 'application/json;'
  };
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

const getToken = () => {
  let token = null;
  if (sessionStorage.getItem(storageKey)) {
    token = JSON.parse(sessionStorage.getItem(storageKey))['token'];
  }
  return token;
};

const getRefreshToken = () => {
  let refreshToken = null;
  if (sessionStorage.getItem(storageKey)) {
    refreshToken = JSON.parse(sessionStorage.getItem(storageKey))['refreshToken'];
  }
  return refreshToken;
};

const logHttpResponseError = async response => {
  const contentType = response.headers.get('content-type');
  if (contentType && contentType.indexOf('application/json') !== -1) {
    try {
      const json = await response.json();
      console.error('status: ' + json.status + ' message: ' + json.message);
    } catch (error) {
      console.error('status: ' + response.status + ' message: ' + response.message);
    }
  } else {
    console.error('status: ' + response.status + ' message: ' + response.message);
  }
};