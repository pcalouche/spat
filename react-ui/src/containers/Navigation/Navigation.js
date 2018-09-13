import React, {Component}                                           from 'react';
import {Link, NavLink as RRNavLink, Redirect, Route, Switch}        from 'react-router-dom';
import {Collapse, Nav, Navbar, NavbarBrand, NavbarToggler, NavLink} from 'reactstrap';

import './Navigation.css';
import Login                                                        from '../Login/Login';
import UserList                                                     from '../UserList/UserList';
import TeamList                                                     from '../TeamList/TeamList';

class Navigation extends Component {
  state = {
    isOpen: false,
    auth: true,
    loggedInUser: 'activeAdmin'
  };

  toggle = () => {
    this.setState({isOpen: !this.state.isOpen});
  };

  render() {
    return (
      <div className="Navigation">
        <Navbar color="primary" dark expand="md">
          <NavbarBrand href="/">SPAT</NavbarBrand>
          <NavbarToggler onClick={this.toggle}/>
          <Collapse isOpen={this.state.isOpen} navbar>
            <Nav className="mr-auto" navbar>
              <NavLink to="/teams" activeClassName="active" tag={RRNavLink}>Teams</NavLink>
              <NavLink to="/users" activeClassName="active" tag={RRNavLink}>Users</NavLink>
            </Nav>
            <Nav navbar>
              <NavLink to="/login" tag={Link}>Logout</NavLink>
              {this.state.auth && <span className="navbar-text"> | {this.state.loggedInUser}</span>}
            </Nav>
          </Collapse>
        </Navbar>
        <Switch>
          <Route exact path="/login" component={() => <Login/>}/>
          <Route exact path="/users" component={() => <UserList/>}/>
          <Route exact path="/teams" component={() => <TeamList/>}/>
          <Redirect from="/" exact to="/login"/>
          <Route render={() => <h1>Not found</h1>}/>
        </Switch>
      </div>
    );
  }
}

export default Navigation;