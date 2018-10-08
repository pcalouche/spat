import React, {Component}                       from 'react';
import {BrowserRouter}                          from 'react-router-dom';
import {Provider}                               from 'react-redux';
import {createStore}                            from 'redux';
import {library}                                from '@fortawesome/fontawesome-svg-core';
import {faPencilAlt, faStroopwafel, faTrashAlt} from '@fortawesome/free-solid-svg-icons';

import './App.css';
import rootReducer                              from 'store/rootReducer';
import Navigation                               from './containers/Navigation/Navigation';

library.add(faPencilAlt, faTrashAlt, faStroopwafel);

const store = createStore(rootReducer);

class App extends Component {
  render() {
    return (
      <Provider store={store}>
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
