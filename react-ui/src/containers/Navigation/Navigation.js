import React, {Component}                 from 'react';
import {NavLink, Redirect, Route, Switch} from 'react-router-dom';

import './Navigation.css';
import Login                              from '../Login/Login';
import UserList                           from '../UserList/UserList';
import TeamList                           from '../TeamList/TeamList';

class Navigation extends Component {
  render() {
    return (
      <div className="Navigation">
        <header>
          <nav>
            <ul>
              <li><NavLink to="/login" activeClassName={'active-link'}>Login</NavLink></li>
              <li><NavLink to="/users" activeClassName={'active-link'}>Users</NavLink></li>
              <li><NavLink to="/teams" activeClassName={'active-link'}>Teams</NavLink></li>
            </ul>
          </nav>
        </header>
        <Switch>
          <Route exact path="/login" component={() => <Login/>}/>
          <Route exact path="/users" component={() => <UserList/>}/>
          <Route exact path="/teams" component={() => <TeamList/>}/>
          <Redirect from="/" exact to="/users"/>
          <Route render={() => <h1>Not found</h1>}/>
        </Switch>
      </div>
    );
  }
}

export default Navigation;