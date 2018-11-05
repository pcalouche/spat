import {apiUtils, userApi} from '../../rest-api';

export const LOAD_USERS = 'LOAD_USERS';
export const SHOW_USERS = 'SHOW_USERS';
export const HANDLE_USER_LIST_LOAD_ERROR = 'HANDLE_USER_LIST_LOAD_ERROR';
export const ADD_USER = 'ADD_USER';
export const EDIT_USER = 'EDIT_USER';
export const DELETE_USER = 'DELETE_USER';

export const loadUsers = () => async dispatch => {
  dispatch({type: LOAD_USERS, loading: true});
  try {
    const users = await userApi.fetchUsers();
    dispatch({type: SHOW_USERS, list: users});
  } catch (error) {
    apiUtils.logError(error);
    dispatch({type: HANDLE_USER_LIST_LOAD_ERROR});
  }
};