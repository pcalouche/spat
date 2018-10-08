import * as userActionType from './userActions';

const initialState = {
  list: [],
  selected: null
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case userActionType.ADD_USER:
      return {
        ...state
      };
    default:
      return state;
  }
};

export default reducer;