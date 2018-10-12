import * as actionType from '../actions/userActions';

const initialState = {
    list: [],
    selected: null
};

const reducer = (state = initialState, action) => {
    switch (action.type) {
        case actionType.LOAD_USERS:
            return {
                ...state,
                list: action.list
            };
        case actionType.ADD_USER:
            return {
                ...state
            };
        default:
            return state;
    }
};

export default reducer;