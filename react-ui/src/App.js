import React, {Component}                       from 'react';
import {BrowserRouter}                          from 'react-router-dom';
import {library}                                from '@fortawesome/fontawesome-svg-core';
import {faPencilAlt, faStroopwafel, faTrashAlt} from '@fortawesome/free-solid-svg-icons';

import './App.css';
import Navigation                               from './containers/Navigation/Navigation';

library.add(faPencilAlt, faTrashAlt, faStroopwafel);

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
