import * as api     from '../../rest-api/Api';
import * as teamApi from '../../rest-api/TeamApi';

export const LOAD_TEAMS = 'LOAD_TEAMS';
export const ADD_TEAM = 'ADD_TEAM';
export const EDIT_TEAM = 'EDIT_TEAM';
export const DELETE_TEAM = 'DELETE_TEAM';

export const loadTeams = () => async dispatch => {
    try {
        const teams = await teamApi.fetchTeams();
        dispatch({type: LOAD_TEAMS, list: teams});
    } catch (error) {
        api.logError(error);
        dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
    }
};