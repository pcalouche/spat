import {apiUtils, userApi} from '../../rest-api';

export const LOAD_USERS = 'LOAD_USERS';
export const SHOW_USERS = 'SHOW_USERS';
export const HANDLE_USER_LIST_LOAD_ERROR = 'HANDLE_USER_LIST_LOAD_ERROR';
export const SHOW_USER_MODAL = 'SHOW_USER_MODAL';
export const SHOW_USER_MODAL_ERROR = 'SHOW_USER_MODAL_ERROR';
export const HIDE_USER_MODAL = 'HIDE_USER_MODAL';
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

export const showUserModal = (mode, user, submitCallback) => dispatch => {
  dispatch({type: SHOW_USER_MODAL, mode: mode, user: user, submitCallback: submitCallback});
};

export const hideUserModal = () => dispatch => {
  dispatch({type: HIDE_USER_MODAL});
};

export const addUser = (user) => async dispatch => {
  try {
    user = await userApi.addUser(user);
    dispatch({type: ADD_USER, user: user});
  } catch (error) {
    apiUtils.logError(error);
    dispatch({
      type: SHOW_USER_MODAL_ERROR,
      message: `Unable to add ${user.username}.  Please try again later.`
    });
  }
};

export const editUser = (user) => async dispatch => {
  try {
    user = await userApi.editUser(user);
    dispatch({type: EDIT_USER, user: user});
  } catch (error) {
    apiUtils.logError(error);
    dispatch({
      type: SHOW_USER_MODAL_ERROR,
      message: `Unable to edit ${user.username}.  Please try again later.`
    });
  }
};

export const deleteUser = (user) => async dispatch => {
  try {
    await userApi.deleteUser(user);
    dispatch({type: DELETE_USER, user: user});
  } catch (error) {
    apiUtils.logError(error);
    dispatch({
      type: SHOW_USER_MODAL_ERROR,
      message: `Unable to delete ${user.username}.  Please try again later.`
    });
  }
};