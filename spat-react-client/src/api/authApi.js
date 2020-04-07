import config from '../config';
import * as apiHelper from './apiHelper';

export const login = async ({username, password}) => {
  const response = await fetch(`${config.apiUrl}/auth/token`, {
    method: 'POST',
    headers: {
      ...apiHelper.jsonHeader,
      ...apiHelper.basicAuthHeader(username, password)
    }
  });
  return apiHelper.handleJsonResponse(response);
};
