export const jsonHeader = {
  'Content-Type': 'application/json;'
};

export const basicAuthHeader = (username, password) => {
  return {
    'Authorization': 'Basic ' + btoa(username + ':' + password)
  };
};

export const jwtHeader = () => {
  return {
    'Authorization': 'Bearer ' + localStorage.getItem('token')
  };
};

export const handleJsonResponse = async (response) => {
  if (response.ok) {
    // Update last activity on successful server responses
    localStorage.setItem('lastActivity', new Date().toISOString());
    return response.json();
  } else {
    return response.json().catch(err => {
      // the status was not ok and there is no json body
      throw new Error(err.responseText);
    }).then(json => {
      // the status was not ok and there is a json body
      throw json;
    });
  }
};

export const handleTextResponse = async response => {
  if (response.ok) {
    return response.text();
  }
  if (!response.ok) {
    return response.json().catch(err => {
      // the status was not ok and there is no json body
      throw new Error(err.responseText);
    }).then(json => {
      // the status was not ok and there is a json body
      throw json;
    });
  }
};

export const handleEmptyResponse = async response => {
  if (!response.ok) {
    return response.json().catch(err => {
      // the status was not ok and there is no json body
      throw new Error(err.responseText);
    }).then(json => {
      // the status was not ok and there is a json body
      throw json;
    });
  }
};