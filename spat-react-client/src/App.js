import React, {useCallback, useEffect, useState} from 'react';
import {Redirect, Route, Switch, useHistory} from 'react-router-dom';
import {library} from '@fortawesome/fontawesome-svg-core';
import {faPencilAlt, faStroopwafel, faTrashAlt} from '@fortawesome/free-solid-svg-icons';

import {userApi} from './api';
import {useAppContext} from './hooks';
import {AppBar} from './components';
import {Login, Teams, Users} from './containers';

// Add Font Awesome icons
library.add(faPencilAlt, faTrashAlt, faStroopwafel);

const App = () => {
  const history = useHistory();
  const {currentUser, setCurrentUser} = useAppContext();
  const [loading, setLoading] = useState(true);

  // TODO implement JWT monitoring

  const PageNotFound = () => (<h1>Page Not Found</h1>);

  const authenticate = useCallback(
    async () => {
      if (localStorage.getItem('token')) {
        try {
          const currentUser = await userApi.currentUser();
          setCurrentUser(currentUser);
          setLoading(false);
        } catch (error) {
          localStorage.clear();
          setCurrentUser(undefined);
          history.push('/login');
          setLoading(false);
        }
      } else {
        setLoading(false);
        history.push('/login');
      }
    },
    [setCurrentUser, history]
  );

  useEffect(() => {
    authenticate();
  }, [authenticate]);

  const renderRoutes = () => {
    return loading ? null
      :
      currentUser ?
        (
          <Switch>
            <Redirect exact from="/" to="/users"/>
            <Redirect exact from="/login" to="/users"/>
            <Route exact path="/users" component={Users}/>
            <Route exact path="/teams" component={Teams}/>
            <Route render={PageNotFound}/>
          </Switch>
        )
        :
        (
          <Switch>
            <Redirect exact from="/" to="/login"/>
            <Route exact path="/login" component={Login}/>
            <Route render={PageNotFound}/>
          </Switch>
        );
  };

  return (
    <div className="App">
      <AppBar/>
      {renderRoutes()}
    </div>
  );
};

export default App;
