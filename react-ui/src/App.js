import React, {Component} from 'react';
import {BrowserRouter} from 'react-router-dom';
import {Provider} from 'react-redux';
import {applyMiddleware, compose, createStore} from 'redux';
import thunk from 'redux-thunk';
import {library} from '@fortawesome/fontawesome-svg-core';
import {faPencilAlt, faStroopwafel, faTrashAlt} from '@fortawesome/free-solid-svg-icons';

import './App.css';
import rootReducer from 'store/reducers/rootReducer';
import Navigation from './containers/Navigation/Navigation';

library.add(faPencilAlt, faTrashAlt, faStroopwafel);

const logger = (store) => {
    return next => {
        return action => {
            console.log('[Middleware] Dispatching', action);
            const result = next(action);
            console.log('[Middleware] next state', store.getState());
            return result;
        };
    };
};

const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;
const rootStore = createStore(rootReducer, composeEnhancers(applyMiddleware(thunk)));
// const rootStore = createStore(rootReducer, composeEnhancers(applyMiddleware(logger, thunk)));

// const rootStore = createStore(rootReducer, composeEnhancers(applyMiddleware(logger)));

class App extends Component {
    render() {
        return (
            <Provider store={rootStore}>
                <BrowserRouter basename="/spat/ui/react">
                    <div className="App">
                        <Navigation/>
                    </div>
                </BrowserRouter>
            </Provider>
        );
    }
}

export default App;
