import {TOKEN} from '../util/SecurityUtils';

// export const fetchTeams = () => {
//   // return fetch('http://localhost:10000/spat/rest-services/api/teams', {
//   //   headers: jsonHeaders()
//   return fetch('http://localhost:10000/spat/rest-services/api/teams/42', {
//     method: 'DELETE',
//     headers: jsonHeaders()
//     // return fetch('http://localhost:10000/spat/rest-services/api/fake', {
//     //   headers: jsonHeaders()
//     // return fetch('http://localhost:10005/spat/rest-services/api/teams', {
//     //   headers: jsonHeaders()
//   }).then(handleJsonResponse);
// };

export const fetchTeams = async () => {
  // const response = await fetch('http://localhost:10000/spat/rest-services/api/teams', {
  //   headers: jsonHeaders()
  // const response = await fetch('http://localhost:10000/spat/rest-services/api/teams/42', {
  //   method: 'DELETE',
  //   headers: jsonHeaders()
  // const response = await fetch('http://localhost:10000/spat/rest-services/api/fake', {
  //   headers: jsonHeaders()
  const response = await fetch('http://localhost:10005/spat/rest-services/api/teams', {
    headers: jsonHeaders()
  });
  return handleJsonResponse(response);
};

// export const handleJsonResponse = (response) => {
//   if (!response.ok) {
//     logHttpResponseError(response);
//     throw new Error('HTTP response error');
//   }
//   return response.json();
// };

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

// const logHttpResponseError = response => {
//   const contentType = response.headers.get('content-type');
//   if (contentType && contentType.indexOf('application/json') !== -1) {
//     response.json()
//       .then(json => {
//         console.error('status: ' + json.status + ' message: ' + json.message);
//       });
//   } else {
//     console.error('status: ' + response.status + ' message: ' + response.message);
//   }
// };

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

const jsonHeaders = () => {
  return {
    'Authorization': 'Bearer ' + TOKEN,
    'Content-Type': 'application/json;'
  };
};