import {userActions} from '../actions';

const initialState = {
  loading: true,
  list: [],
  showError: false,
  selectedUser: {},
  modalIsOpen: false,
  modalMode: 'Add',
  modalError: null,
  modalSubmitCallback: () => {
  }
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
    case userActions.SHOW_USER_MODAL: {
      return {
        ...state,
        modalIsOpen: true,
        modalError: null,
        modalMode: action.mode,
        selectedUser: action.user,
        modalSubmitCallback: action.submitCallback
      };
    }
    case userActions.SHOW_USER_MODAL_ERROR: {
      return {
        ...state,
        modalError: action.message
      };
    }
    case userActions.HIDE_USER_MODAL: {
      return {
        ...state,
        modalIsOpen: false
      };
    }
    case userActions.ADD_USER:
      return {
        ...state,
        list: [...state.list, action.user],
        modalIsOpen: false
      };
    case userActions.EDIT_USER:
      const newList = [...state.list];
      newList[newList.findIndex(el => el.username === action.user.username)] = action.user;
      return {
        ...state,
        list: newList,
        modalIsOpen: false
      };
    case userActions.DELETE_USER: {
      return {
        ...state,
        list: state.list.filter((user) => action.user.username !== user.username),
        modalIsOpen: false
      };
    }
    default:
      return state;
  }
};

export default reducer;