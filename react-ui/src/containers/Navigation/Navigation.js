import React, {Component}                                                from 'react';
import {connect}                                                         from 'react-redux';
import {Link, NavLink as RRNavLink, Redirect, Route, Switch, withRouter} from 'react-router-dom';
import {Collapse, Nav, Navbar, NavbarBrand, NavbarToggler, NavLink}      from 'reactstrap';

import './Navigation.scss';
import Login                                                             from '../Login/Login';
import TeamList                                                          from '../TeamList/TeamList';
import UserList                                                          from '../UserList/UserList';
import BasicModal                                                        from '../../components/BasicModal';
import {authActions}                                                     from '../../redux/actions';

class Navigation extends Component {

  constructor(props) {
    super(props);
    this.state = {
      isOpen: false,
      lastActivity: new Date()
    };
  }

  handleClick = () => {
    this.props.updateLastActivity();
    this.setState({lastActivity: new Date()});
  };

  toggle = () => {
    this.setState({isOpen: !this.state.isOpen});
  };

  monitorUserInactivity = () => {
    if (this.props.loggedInUser) {
      const nowTime = new Date().getTime();
      const inactiveTime = (nowTime - this.state.lastActivity.getTime()) / 1000;
      const expirationTime = this.props.tokenClaims.exp * 1000;
      const timeLeft = (expirationTime - nowTime) / 1000;
      // Duration is in seconds.  It is the expiration time minus the issued at time
      const tokenDuration = this.props.tokenClaims.exp - this.props.tokenClaims.iat;
      console.log(`inactiveTime-> ${inactiveTime}, time left-> ${timeLeft} seconds, duration-> ${tokenDuration}`);

      // Refresh the token if it is to expire in the next 60 seconds.  The next set of checks will check for user inactivity.
      // User inactivity is the ultimate indicator when deciding to prompt the user if they want to extend their session or
      // to log them out all together.
      if (timeLeft <= 60) {
        console.log('refreshing token');
        this.props.refreshToken();
      }

      // If there is less than 60 seconds left before the token expires or if the user has been inactive for the token's
      // duration minus 60 seconds then show a modal letting the user know their session is about to expire.
      console.log('inactiveTime >= tokenDuration - 60,', inactiveTime >= tokenDuration - 60);
      if (!this.props.showLogoutWarningModal && inactiveTime >= tokenDuration - 60) {
        this.props.showLogoutWarning();
      }

      // If then token will expire before the next interval check or if the user is likely to still be inactive before the next interval check,
      // then log the user out and display a modal letting them know why they were logged out.  Even though we refresh the token when it is
      // set to expire in the next 60 seconds, it is quite possible we don't get a refresh token if the server is down or there is a network
      // issue that is preventing the client from communicating with the server.
      if (timeLeft <= 10 && tokenDuration <= (inactiveTime + 10)) {
        this.props.forceLogout();
      }
    }

    // Restart timeout for monitoring the login
    setTimeout(() => {
      this.monitorUserInactivity();
    }, 1000 * 10);
  };


  setDocumentTitle = (baseTitle) => {
    switch (this.props.location.pathname) {
      case '/login':
        document.title = baseTitle + ' - Login';
        break;
      case '/teams':
        document.title = baseTitle + ' - Teams';
        break;
      case '/users':
        document.title = baseTitle + ' - Users';
        break;
      default :
        document.title = baseTitle;
        break;
    }
  };

  componentDidMount = async () => {
    this.lastActivity = new Date();
    if (this.props.token) {
      try {
        await this.props.loginUserByExistingToken();
      } catch (error) {
        console.error('Error processing existing token. Will perform logout now.');
        this.props.logoutUser();
      }
    }
    this.setDocumentTitle('SPAT React UI');
    this.monitorUserInactivity();
    this.props.updateLastActivity();
  };

  componentDidUpdate = (prevProps) => {
    this.setDocumentTitle('SPAT React UI');
  };

  shouldComponentUpdate(nextProps, nextState) {
    return !(this.state.lastActivity !== nextState.lastActivity && this.props.location === nextProps.location);

  };

  render() {
    // Wait for existing token to be evaluated
    if (this.props.token && !this.props.loggedInUser) {
      return null;
    }
    return (
        <div className="Navigation" onClick={this.handleClick}>
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
          <BasicModal
              open={this.props.showExpirationModal}
              title="Logout Warning"
              message="You will be logged out shortly from inactivity.  Click OK to stay logged in."
              submitCallback={async () => {
                await this.props.refreshToken();
                this.setState({lastActivity: new Date()});
              }}
              cancelCallback={this.props.dismissLogoutWarning}/>
          <BasicModal
              open={this.props.showLoggedOutModal}
              title="Logged Out"
              message="You were logged out due to inactivity."
              submitCallback={this.props.acknowledgeLogout}
              showCancelButton={false}/>
        </div>
    );
  }
}

const mapStateToProps = (state) => {
  return {
    token: state.auth.token,
    tokenClaims: state.auth.tokenClaims,
    loggedInUser: state.auth.loggedInUser,
    lastActivity: state.auth.lastActivity,
    showExpirationModal: state.auth.showExpirationModal,
    showLoggedOutModal: state.auth.showLoggedOutModal
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    loginUserByExistingToken: () => dispatch(authActions.loginUserByExistingToken()),
    updateLastActivity: () => dispatch(authActions.updateLastActivity()),
    refreshToken: () => dispatch(authActions.refreshToken()),
    showLogoutWarning: () => dispatch(authActions.showLogoutWarning()),
    dismissLogoutWarning: () => dispatch(authActions.dismissLogoutWarning()),
    forceLogout: () => dispatch(authActions.forceLogout()),
    acknowledgeLogout: () => dispatch(authActions.acknowledgeLogout()),
    logoutUser: () => dispatch(authActions.logoutUser())
  };
};

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(Navigation));