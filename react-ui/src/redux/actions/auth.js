import {apiUtils, authApi, userApi} from '../../rest-api';

export const DO_SUCCESSFUL_LOGIN = 'DO_SUCCESSFUL_LOGIN';
export const DO_BAD_LOGIN = 'DO_BAD_LOGIN';
export const LOGIN_BY_EXISTING_TOKEN = 'LOGIN_BY_EXISTING_TOKEN';
export const LOGOUT_USER = 'LOGOUT_USER';

export const loginUser = (username, password) => async dispatch => {
  try {
    const tokenData = await authApi.fetchToken(username, password);
    if (tokenData.token) {
      sessionStorage.setItem(apiUtils.storageKey, JSON.stringify(tokenData));
      const user = await userApi.fetchUser(username);
      user.roles = user.roles.map(role => role.name);
      dispatch({type: DO_SUCCESSFUL_LOGIN, tokenData: tokenData, user: user});
    } else {
      dispatch({type: DO_BAD_LOGIN, errorMessage: tokenData.message});
    }
  } catch (error) {
    apiUtils.logError(error);
    dispatch({type: DO_BAD_LOGIN, errorMessage: 'Unable to connect at this time.'});
  }
};

export const loginUserByExistingToken = () => async dispatch => {
  try {
    const user = await userApi.fetchCurrentUser();
    user.roles = user.roles.map(role => role.name);
    dispatch({type: LOGIN_BY_EXISTING_TOKEN, user: user});
  } catch (error) {
    
  }
};

export const logoutUser = () => dispatch => {
  dispatch({type: LOGOUT_USER});
};