import {applyMiddleware, combineReducers, compose, createStore} from 'redux';
import thunk                                                    from 'redux-thunk';

import * as reducers from './reducers';

const rootReducer = combineReducers({
  auth: reducers.authReducer,
  teams: reducers.teamReducer,
  users: reducers.userReducer
});

const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;

export default createStore(rootReducer, composeEnhancers(applyMiddleware(thunk)));