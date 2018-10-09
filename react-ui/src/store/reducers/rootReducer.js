import {combineReducers} from 'redux';

import teams from './teamReducer';
import users from './userReducer';

export default combineReducers({
  teams: teams,
  users: users
});