import {apiUtils, teamApi} from '../../rest-api';

export const LOAD_TEAMS = 'LOAD_TEAMS';
export const ADD_TEAM = 'ADD_TEAM';
export const EDIT_TEAM = 'EDIT_TEAM';
export const DELETE_TEAM = 'DELETE_TEAM';

export const loadTeams = () => async dispatch => {
  try {
    const teams = await teamApi.fetchTeams();
    dispatch({type: LOAD_TEAMS, list: teams});
  } catch (error) {
    apiUtils.logError(error);
    dispatch({type: LOAD_TEAMS, list: [{id: 1, name: 'Error team'}]});
  }
};