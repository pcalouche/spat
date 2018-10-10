import React                                   from 'react';
import ReactDOM                                from 'react-dom';
import {applyMiddleware, compose, createStore} from 'redux';
import {Provider}                              from 'react-redux';
import thunk                                   from 'redux-thunk';
import {BrowserRouter}                         from 'react-router-dom';
import registerServiceWorker                   from './registerServiceWorker';
import 'bootstrap/dist/css/bootstrap.css';

import './index.css';
import rootReducer                             from './store/reducers/rootReducer';
import App                                     from './App';

// const logger = (store) => {
//     return next => {
//         return action => {
//             console.log('[Middleware] Dispatching', action);
//             const result = next(action);
//             console.log('[Middleware] next state', store.getState());
//             return result;
//         };
//     };
// };

const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;
// const rootStore = createStore(rootReducer, composeEnhancers(applyMiddleware(logger)));
// const rootStore = createStore(rootReducer, composeEnhancers(applyMiddleware(logger, thunk)));
const rootStore = createStore(rootReducer, composeEnhancers(applyMiddleware(thunk)));

ReactDOM.render(
  <Provider store={rootStore}>
    <BrowserRouter basename="/spat/ui/react">
      <App/>
    </BrowserRouter>
  </Provider>,
  document.getElementById('root'));
registerServiceWorker();
