import React                 from 'react';
import ReactDOM              from 'react-dom';
import {Provider}            from 'react-redux';
import {BrowserRouter}       from 'react-router-dom';
import registerServiceWorker from './registerServiceWorker';
import 'react-app-polyfill/ie11';

import './index.css';
// import 'bootstrap-material-design/dist/css/bootstrap-material-design.css';
import 'bootstrap/dist/css/bootstrap.css';
import store                 from './redux/store';
import App                   from './App';

ReactDOM.render(
  <Provider store={store}>
    <BrowserRouter basename="/spat/ui/react">
      <App/>
    </BrowserRouter>
  </Provider>,
  document.getElementById('root'));
registerServiceWorker();
