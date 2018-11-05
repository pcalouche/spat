import React, {Component}                                                from 'react';
import {connect}                                                         from 'react-redux';
import {Link, NavLink as RRNavLink, Redirect, Route, Switch, withRouter} from 'react-router-dom';
import {Collapse, Nav, Navbar, NavbarBrand, NavbarToggler, NavLink}      from 'reactstrap';

import './Navigation.css';
import Login                                                             from '../Login/Login';
import TeamList                                                          from '../TeamList/TeamList';
import {authActions}                                                     from '../../redux/actions';

import UserList from '../UserList/UserList';

class Navigation extends Component {
  baseTitle = 'SPAT React UI';

  state = {
    isOpen: false
  };

  toggle = () => {
    this.setState({isOpen: !this.state.isOpen});
  };

  setDocumentTitle = () => {
    switch (this.props.location.pathname) {
      case '/login':
        document.title = this.baseTitle + ' - Login';
        break;
      case '/teams':
        document.title = this.baseTitle + ' - Teams';
        break;
      case '/users':
        document.title = this.baseTitle + ' - Users';
        break;
      default :
        document.title = this.baseTitle;
        break;
    }
  };

  componentDidMount = () => {
    if (this.props.token) {
      this.props.loginUserByExistingToken();
    }
    this.setDocumentTitle();
  };

  componentDidUpdate = () => {
    this.setDocumentTitle();
  };

  render() {
    // Wait for existing token to be evaluated
    if (this.props.token && !this.props.loggedInUser) {
      return null;
    }
    return (
      <div className="Navigation">
        <Navbar color="primary" dark expand="md">
          <NavbarBrand tag={'span'}>SPAT</NavbarBrand>
          <NavbarToggler onClick={this.toggle}/>
          {this.props.loggedInUser &&
          <Collapse isOpen={this.state.isOpen} navbar>
            <Nav className="mr-auto" navbar>
              <NavLink to="/teams" activeClassName="active" tag={RRNavLink}>Teams</NavLink>
              <NavLink to="/users" activeClassName="active" tag={RRNavLink}>Users</NavLink>
            </Nav>
            <Nav navbar>
              <NavLink to="/login" tag={Link} onClick={this.props.logoutUser}>Logout</NavLink>
              <span className="navbar-text"> | {this.props.loggedInUser.username}</span>
            </Nav>
          </Collapse>
          }
        </Navbar>
        {this.props.loggedInUser &&
        <Switch>
          <Redirect from='/login' to='/teams'/>
          <Route exact path="/teams" component={() => <TeamList/>}/>
          <Route exact path="/users" component={() => <UserList/>}/>
          <Route render={() => <h1>Not found</h1>}/>
        </Switch>
        }
        {!this.props.loggedInUser &&
        <Switch>
          <Route component={() => <Login/>}/>
        </Switch>
        }
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  return {
    token: state.auth.token,
    loggedInUser: state.auth.loggedInUser
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    logoutUser: () => dispatch(authActions.logoutUser()),
    loginUserByExistingToken: () => dispatch(authActions.loginUserByExistingToken())
  };
};

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(Navigation));