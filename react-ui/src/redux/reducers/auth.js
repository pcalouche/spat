import {authActions}  from '../actions';
import {getTokenData} from '../../rest-api/apiUtils';

const spatTokenData = getTokenData();
const initialState = {
  token: spatTokenData == null ? null : spatTokenData.token,
  refreshToken: spatTokenData == null ? null : spatTokenData.refreshToken,
  loggedInUser: null,
  loginError: null,
  tokenClaims: null,
  lastActivity: new Date().getTime(),
  showExpirationModal: false,
  showLoggedOutModal: false
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case authActions.DO_SUCCESSFUL_LOGIN:
      return {
        ...state,
        token: action.tokenData.token,
        refreshToken: action.tokenData.refreshToken,
        tokenClaims: action.tokenClaims,
        loggedInUser: action.user,
        lastActivity: new Date(),
        errorMessage: null,
        showExpirationModal: false
      };
    case authActions.DO_BAD_LOGIN:
      return {
        ...state,
        errorMessage: action.errorMessage
      };
    case authActions.LOGIN_BY_EXISTING_TOKEN:
      return {
        ...state,
        loggedInUser: action.user,
        tokenClaims: action.tokenClaims,
        lastActivity: new Date()
      };
    case authActions.UPDATE_LAST_ACTIVITY:
      return {
        ...state,
        lastActivity: new Date().getTime()
      };
    case authActions.REFRESH_TOKEN:
      return {
        ...state,
        token: action.tokenData.token,
        refreshToken: action.tokenData.refreshToken,
        tokenClaims: action.tokenClaims,
        lastActivity: new Date(),
        errorMessage: null,
        showExpirationModal: false
      };
    case authActions.SHOW_LOGOUT_WARNING:
      return {
        ...state,
        showExpirationModal: true
      };
    case authActions.DISMISS_LOGOUT_WARNING:
      return {
        ...state,
        showExpirationModal: false
      };
    case authActions.FORCE_LOGOUT:
      return {
        ...state,
        token: null,
        loggedInUser: null,
        showLogoutWarningModal: false,
        showLoggedOutModal: true
      };
    case authActions.ACKNOWLEDGE_LOGOUT:
      return {
        ...state,
        showLoggedOutModal: false
      };
    case  authActions.LOGOUT_USER:
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