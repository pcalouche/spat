import React                 from 'react';
import ReactDOM              from 'react-dom';
import {Provider}            from 'react-redux';
import {BrowserRouter}       from 'react-router-dom';
import registerServiceWorker from './registerServiceWorker';
import 'react-app-polyfill/ie11';
import 'bootstrap/dist/css/bootstrap.css';

import './index.css';
import store                 from './store/store';
import App                   from './App';

ReactDOM.render(
    <Provider store={store}>
        <BrowserRouter basename="/spat/ui/react">
            <App/>
        </BrowserRouter>
    </Provider>,
    document.getElementById('root'));
registerServiceWorker();
