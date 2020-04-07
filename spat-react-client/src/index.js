import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter} from 'react-router-dom';

import './index.scss';
import {AppProvider} from './contexts';
import App from './App';

ReactDOM.render(
  <BrowserRouter>
    <AppProvider>
      <App/>
    </AppProvider>
  </BrowserRouter>,
  document.getElementById('root')
);
