import * as teamActionType from './teamActions';

const initialState = {
  list: [{id: 1, name: 'Team 1'}],
  selected: null
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case teamActionType.ADD_TEAM:
      return {
        ...state
      };
    default:
      return state;
  }
};

export default reducer;