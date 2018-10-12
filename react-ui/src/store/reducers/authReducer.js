import * as actionType from '../actions/authActions';
import {storageKey}    from '../../rest-api/Api';

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
        case actionType.DO_SUCCESSFUL_LOGIN:
            return {
                ...state,
                token: action.tokenData.token,
                refreshToken: action.tokenData.refreshToken,
                loggedInUser: action.user,
                errorMessage: null
            };
        case actionType.DO_BAD_LOGIN:
            return {
                ...state,
                errorMessage: action.errorMessage
            };
        case actionType.LOGIN_BY_EXISTING_TOKEN:
            return {
                ...state,
                loggedInUser: action.user
            };
        case  actionType.LOGOUT_USER:
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