import {teamActions} from '../actions';

const initialState = {
  loading: true,
  list: [],
  showError: false,
  selectedTeam: {},
  modalIsOpen: false,
  modalMode: 'Add',
  modalError: null,
  modalCallback: () => {
  }
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case teamActions.LOAD_TEAMS:
      return {
        ...state,
        loading: action.loading,
        showError: false
      };
    case teamActions.SHOW_TEAMS:
      return {
        ...state,
        list: action.list,
        loading: false
      };
    case teamActions.HANDLE_TEAM_LIST_LOAD_ERROR:
      return {
        ...state,
        list: [],
        loading: false,
        showError: true
      };
    case teamActions.SHOW_TEAM_MODAL: {
      console.info(action);
      return {
        ...state,
        modalIsOpen: true,
        modalError: null,
        modalMode: action.mode,
        selectedTeam: action.team,
        modalCallback: action.callback
      };
    }
    case teamActions.SHOW_TEAM_MODAL_ERROR: {
      return {
        ...state,
        modalError: action.message
      };
    }
    case teamActions.HIDE_TEAM_MODAL: {
      return {
        ...state,
        modalIsOpen: false
      };
    }
    case teamActions.ADD_TEAM:
      return {
        ...state
      };
    case teamActions.DELETE_TEAM: {
      return {
        ...state,
        list: state.list.filter((team) => action.team.id !== team.id),
        modalIsOpen: false
      };
    }
    default:
      return state;
  }
};

export default reducer;