import {userActions} from '../actions';

const initialState = {
  list: [],
  selected: null
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case userActions.LOAD_USERS:
      return {
        ...state,
        list: action.list
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