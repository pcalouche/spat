import {baseUrl, handleJsonResponse, jwtHeaders} from './apiUtils';

export const fetchTeams = async () => {
  const response = await fetch(baseUrl + '/teams', {
    headers: jwtHeaders()
  });
  return handleJsonResponse(response);
};

