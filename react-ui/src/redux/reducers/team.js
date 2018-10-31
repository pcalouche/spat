import {teamActions} from '../actions';

const initialState = {
  list: [],
  selected: null
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case teamActions.LOAD_TEAMS:
      return {
        ...state,
        list: action.list
      };
    case teamActions.ADD_TEAM:
      return {
        ...state
      };
    default:
      return state;
  }
};

export default reducer;