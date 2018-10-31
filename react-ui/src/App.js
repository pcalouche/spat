import React                                    from 'react';
import {library}                                from '@fortawesome/fontawesome-svg-core';
import {faPencilAlt, faStroopwafel, faTrashAlt} from '@fortawesome/free-solid-svg-icons';

import Navigation from './containers/Navigation/Navigation';

library.add(faPencilAlt, faTrashAlt, faStroopwafel);

const App = () => {
  return (
    <div className="App">
      <Navigation/>
    </div>
  );
};

export default App;
