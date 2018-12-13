import React              from 'react';
import ReactDOM           from 'react-dom';
import {Provider}         from 'react-redux';
import {BrowserRouter}    from 'react-router-dom';
import * as serviceWorker from './serviceWorker';
import 'react-app-polyfill/ie11';

import './index.scss';
import store              from './redux/store';
import App                from './App';

ReactDOM.render(
  <Provider store={store}>
    <BrowserRouter basename="/spat/ui/react">
      <App/>
    </BrowserRouter>
  </Provider>,
  document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
