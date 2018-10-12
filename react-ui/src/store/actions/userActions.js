import * as api     from '../../rest-api/Api';
import * as userApi from '../../rest-api/UserApi';

export const LOAD_USERS = 'LOAD_USERS';
export const ADD_USER = 'ADD_USER';
export const EDIT_USER = 'EDIT_USER';
export const DELETE_USER = 'DELETE_USER';

export const loadUsers = () => async dispatch => {
    try {
        const users = await userApi.fetchUsers();
        dispatch({type: LOAD_USERS, list: users});
    } catch (error) {
        api.logError(error);
    }
};