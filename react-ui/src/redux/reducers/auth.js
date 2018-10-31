import {authActions} from '../actions';
import {storageKey}  from '../../rest-api/apiUtils';

let spatTokenData = null;
if (sessionStorage.getItem(storageKey)) {
  spatTokenData = JSON.parse(sessionStorage.getItem(storageKey));
}

const initialState = {
  token: spatTokenData == null ? null : spatTokenData.token,
  refreshToken: spatTokenData == null ? null : spatTokenData.refreshToken,
  loggedInUser: null,
  loginError: null
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case authActions.DO_SUCCESSFUL_LOGIN:
      return {
        ...state,
        token: action.tokenData.token,
        refreshToken: action.tokenData.refreshToken,
        loggedInUser: action.user,
        errorMessage: null
      };
    case authActions.DO_BAD_LOGIN:
      return {
        ...state,
        errorMessage: action.errorMessage
      };
    case authActions.LOGIN_BY_EXISTING_TOKEN:
      return {
        ...state,
        loggedInUser: action.user
      };
    case  authActions.LOGOUT_USER:
      sessionStorage.removeItem(storageKey);
      return {
        ...state,
        token: null,
        loggedInUser: null,
        errorMessage: null
      };
    default:
      return state;
  }
};

export default reducer;