import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter} from 'react-router-dom';
import App from './App';
import {AppProvider} from './contexts';

import './index.scss';

ReactDOM.render(
  <BrowserRouter>
    <AppProvider>
      <App/>
    </AppProvider>
  </BrowserRouter>,
  document.getElementById('root')
);
