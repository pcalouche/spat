import React, {useEffect, useState} from 'react';
import {Redirect, Route, Switch, useHistory} from 'react-router-dom';
import {library} from '@fortawesome/fontawesome-svg-core';
import {faPencilAlt, faStroopwafel, faTrashAlt} from '@fortawesome/free-solid-svg-icons';

import {authApi, userApi} from './api';
import {useAppContext} from './hooks';
import {AppBar} from './components';
import {Login, Teams, Users} from './containers';

// Add Font Awesome icons
library.add(faPencilAlt, faTrashAlt, faStroopwafel);


const App = () => {
  const history = useHistory();
  const {currentUser, setCurrentUser} = useAppContext();
  const [loading, setLoading] = useState(true);

  const PageNotFound = () => (<h1>Page Not Found</h1>);

  // Setup session monitoring
  useEffect(() => {
    const interval = authApi.monitorSession();
    return () => clearInterval(interval);
  }, []);


  useEffect(() => {
    const fetchData = async () => {
      const urlSearchParams = new URLSearchParams(window.location.search);
      if (urlSearchParams.get('signOut')) {
        setLoading(false);
      } else {
        try {
          // Attempt to use refresh token cookie to acquire a token
          // User will be directed to login if this fails, otherwise
          // the current user is retrieved from server and set in the
          // application's react context
          await authApi.requestNewToken();
          const currentUser = await userApi.currentUser();
          setCurrentUser(currentUser);
          setLoading(false);
        } catch (error) {
          await authApi.logout();
          setLoading(false);
        }
      }
    };
    fetchData().then();
  }, [setCurrentUser, history]);

  const renderRoutes = () => {
    return loading ?
      null
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
