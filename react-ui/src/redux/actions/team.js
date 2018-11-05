import {apiUtils, teamApi} from '../../rest-api';

export const LOAD_TEAMS = 'LOAD_TEAMS';
export const SHOW_TEAMS = 'SHOW_TEAMS';
export const DISPLAY_ERROR = 'DISPLAY_ERROR';
export const ADD_TEAM = 'ADD_TEAM';
export const EDIT_TEAM = 'EDIT_TEAM';
export const DELETE_TEAM = 'DELETE_TEAM';

export const loadTeams = () => async dispatch => {
    dispatch({type: LOAD_TEAMS, loading: true});
    try {
        const teams = await teamApi.fetchTeams();
        console.info('doing dispatch');
        dispatch({type: SHOW_TEAMS, list: teams});
    } catch (error) {
        apiUtils.logError(error);
        dispatch({type: DISPLAY_ERROR});
    }
};