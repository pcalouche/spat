import {teamActions} from '../actions';

const initialState = {
    loading: true,
    list: [],
    displayError: false,
    selected: null
};

const reducer = (state = initialState, action) => {
    switch (action.type) {
        case teamActions.LOAD_TEAMS:
            return {
                ...state,
                loading: action.loading,
                displayError: false
            };
        case teamActions.SHOW_TEAMS:
            return {
                ...state,
                list: action.list,
                loading: false
            };
        case teamActions.DISPLAY_ERROR:
            return {
                ...state,
                list: [],
                loading: false,
                displayError: true
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