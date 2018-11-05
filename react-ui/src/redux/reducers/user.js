import {userActions} from '../actions';

const initialState = {
  loading: true,
  list: [],
  showError: false
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case userActions.LOAD_USERS:
      return {
        ...state,
        loading: action.loading,
        showError: false
      };
    case userActions.SHOW_USERS:
      return {
        ...state,
        list: action.list,
        loading: false
      };
    case userActions.HANDLE_USER_LIST_LOAD_ERROR:
      return {
        ...state,
        list: [],
        loading: false,
        showError: true
      };
    case userActions.ADD_USER:
      return {
        ...state
      };
    default:
      return state;
  }
};

export default reducer;