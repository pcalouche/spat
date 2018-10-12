import {applyMiddleware, combineReducers, compose, createStore} from 'redux';
import thunk                                                    from 'redux-thunk';

import auth  from './reducers/authReducer';
import teams from './reducers/teamReducer';
import users from './reducers/userReducer';

const rootReducer = combineReducers({
    auth: auth,
    teams: teams,
    users: users
});

const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;

export default createStore(rootReducer, composeEnhancers(applyMiddleware(thunk)));