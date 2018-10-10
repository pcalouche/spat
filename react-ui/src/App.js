import React, {Component}                       from 'react';
import {library}                                from '@fortawesome/fontawesome-svg-core';
import {faPencilAlt, faStroopwafel, faTrashAlt} from '@fortawesome/free-solid-svg-icons';

import './App.css';
import Navigation                               from './containers/Navigation/Navigation';

library.add(faPencilAlt, faTrashAlt, faStroopwafel);

class App extends Component {
  render() {
    return (
      <div className="App">
        <Navigation/>
      </div>
    );
  }
}

export default App;
