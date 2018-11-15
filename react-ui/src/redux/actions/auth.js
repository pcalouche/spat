import {apiUtils, authApi, userApi} from '../../rest-api';

export const DO_SUCCESSFUL_LOGIN = 'DO_SUCCESSFUL_LOGIN';
export const DO_BAD_LOGIN = 'DO_BAD_LOGIN';
export const LOGIN_BY_EXISTING_TOKEN = 'LOGIN_BY_EXISTING_TOKEN';
export const UPDATE_LAST_ACTIVITY = 'UPDATE_LAST_ACTIVITY';
export const REFRESH_TOKEN = 'REFRESH_TOKEN';
export const SHOW_LOGOUT_WARNING = 'SHOW_LOGOUT_WARNING';
export const DISMISS_LOGOUT_WARNING = 'DISMISS_LOGOUT_WARNING';
export const FORCE_LOGOUT = 'FORCE_LOGOUT';
export const ACKNOWLEDGE_LOGOUT = 'ACKNOWLEDGE_LOGOUT';
export const LOGOUT_USER = 'LOGOUT_USER';

export const loginUser = (username, password) => async dispatch => {
  try {
    const tokenData = await authApi.fetchToken(username, password);
    if (tokenData.token) {
      apiUtils.setToken(tokenData);
      const user = await userApi.fetchUser(username);
      user.roles = user.roles.map(role => role.name);
      dispatch({
        type: DO_SUCCESSFUL_LOGIN,
        tokenData: tokenData,
        user: user,
        tokenClaims: apiUtils.getClaims()
      });
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
    dispatch({
      type: LOGIN_BY_EXISTING_TOKEN,
      user: user,
      tokenClaims: apiUtils.getClaims()
    });
  } catch (error) {
    throw error;
  }
};

export const updateLastActivity = () => dispatch => {
  dispatch({type: UPDATE_LAST_ACTIVITY});
};

export const refreshToken = () => async dispatch => {
  try {
    const tokenData = await authApi.refreshToken();
    apiUtils.setToken(tokenData);
    dispatch({
      type: REFRESH_TOKEN,
      tokenData: tokenData,
      tokenClaims: apiUtils.getClaims()
    });
  } catch (error) {
    console.error('Could not refresh token');
  }
};

export const showLogoutWarning = () => dispatch => {
  dispatch({type: SHOW_LOGOUT_WARNING});
};

export const dismissLogoutWarning = () => dispatch => {
  dispatch({type: DISMISS_LOGOUT_WARNING});
};

export const forceLogout = () => dispatch => {
  apiUtils.clearToken();
  dispatch({type: FORCE_LOGOUT});
};

export const acknowledgeLogout = () => dispatch => {
  dispatch({type: ACKNOWLEDGE_LOGOUT});
};

export const logoutUser = () => dispatch => {
  apiUtils.clearToken();
  dispatch({type: LOGOUT_USER});
};
