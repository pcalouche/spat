import React, {useState} from 'react';
import {Link, NavLink as RRNavLink, useHistory} from 'react-router-dom';
import {Collapse, Nav, Navbar, NavbarBrand, NavbarText, NavbarToggler, NavLink} from 'reactstrap';

import {useAppContext} from '../hooks';
import {authApi} from '../api';

const AppBar = () => {
  const history = useHistory();
  const {currentUser, setCurrentUser} = useAppContext();
  const [isOpen, setIsOpen] = useState(false);

  const logoutHandler = async () => {
    try {
      await authApi.logout();
    } catch (e) {
      // do nothing if we can't communicate with server during logout
    } finally {
      localStorage.clear();
      setCurrentUser(undefined);
      history.push('/login');
    }
  };

  if (!currentUser) {
    return (
      <Navbar aria-label="navigation"
              role="navigation"
              color="primary"
              dark
              expand="md">
        <NavbarBrand tag="span">SPAT</NavbarBrand>
      </Navbar>
    );
  } else {
    return (
      <Navbar aria-label="navigation"
              role="navigation"
              color="primary"
              dark
              expand="md">
        <NavbarBrand tag="span">SPAT</NavbarBrand>
        <NavbarToggler onClick={() => setIsOpen((isOpen => !isOpen))}/>
        <Collapse isOpen={isOpen} navbar>
          <Nav className="mr-auto" navbar>
            <NavLink to="/users" activeClassName="active" tag={RRNavLink}>Users</NavLink>
            <NavLink to="/teams" activeClassName="active" tag={RRNavLink}>Teams</NavLink>
          </Nav>
          <Nav navbar>
            <NavLink to="#" tag={Link} onClick={logoutHandler}>Logout</NavLink>
            <NavbarText>| {currentUser.username}</NavbarText>
          </Nav>
        </Collapse>
      </Navbar>
    );
  }
};

export default AppBar;