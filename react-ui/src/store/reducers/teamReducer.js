import * as teamActionType from '../actions/teamActions';

const initialState = {
    list: [],
    selected: null
};

const reducer = (state = initialState, action) => {
    switch (action.type) {
        case teamActionType.LOAD_TEAMS:
            return {
                ...state,
                list: action.list
            };
        case teamActionType.ADD_TEAM:
            return {
                ...state
            };
        default:
            return state;
    }
};

export default reducer;