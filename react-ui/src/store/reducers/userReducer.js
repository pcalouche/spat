import * as userActionType from '../actions/userActions';

const initialState = {
    list: [],
    selected: null
};

const reducer = (state = initialState, action) => {
    switch (action.type) {
        case userActionType.LOAD_USERS:
            console.log('in load users');
            return state;
        case userActionType.ADD_USER:
            return {
                ...state
            };
        default:
            return state;
    }
};

export default reducer;