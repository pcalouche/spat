import React, {useState} from 'react';
import {Link, NavLink as RRNavLink} from 'react-router-dom';
import {Nav, Navbar} from 'react-bootstrap';

import {authApi} from '../api';
import {useAppContext} from '../hooks';

const AppBar: React.FC = () => {
  const {currentUser, setCurrentUser} = useAppContext();
  const [isOpen, setIsOpen] = useState(false);

  const toggleNavBar = () => {
    setIsOpen(isOpen => !isOpen);
  };

  const logoutHandler = async () => {
    await authApi.logout();
    setCurrentUser(null);
  };

  if (!currentUser) {
    return (
      <Navbar aria-label="navigation"
              role="navigation"
              bg="primary"
              variant="dark"
              expand="md">
        <Navbar.Brand>SPAT</Navbar.Brand>
      </Navbar>
    );
  } else {
    return (
      <Navbar aria-label="navigation"
              role="navigation"
              variant="dark"
              bg="primary"
              expand="lg"
              expanded={isOpen}>
        <Navbar.Brand>SPAT</Navbar.Brand>
        <Navbar.Toggle aria-controls="responsive-navbar-nav" onClick={toggleNavBar}/>
        <Navbar.Collapse id="responsive-navbar-nav">
          <Nav className="mr-auto">
            <Nav.Link to="/users" activeClassName="active" as={RRNavLink} onClick={toggleNavBar}>Users</Nav.Link>
            <Nav.Link to="/teams" activeClassName="active" as={RRNavLink} onClick={toggleNavBar}>Teams</Nav.Link>
          </Nav>
          <Nav>
            <Nav.Link to="#" as={Link} onClick={logoutHandler}>Logout</Nav.Link>
            <Navbar.Text>| {currentUser.username}</Navbar.Text>
          </Nav>
        </Navbar.Collapse>
      </Navbar>
    );
  }
};

export default AppBar;