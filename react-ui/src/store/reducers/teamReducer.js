import * as actionType from '../actions/teamActions';

const initialState = {
    list: [],
    selected: null
};

const reducer = (state = initialState, action) => {
    switch (action.type) {
        case actionType.LOAD_TEAMS:
            return {
                ...state,
                list: action.list
            };
        case actionType.ADD_TEAM:
            return {
                ...state
            };
        default:
            return state;
    }
};

export default reducer;