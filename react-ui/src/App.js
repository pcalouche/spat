import React, {Component} from 'react';
import {BrowserRouter}    from 'react-router-dom';

import './App.css';
import Navigation         from './containers/Navigation/Navigation';

class App extends Component {
  render() {
    return (
      <BrowserRouter>
        <div className="App">
          <Navigation/>
        </div>
      </BrowserRouter>
    );
  }
}

export default App;
