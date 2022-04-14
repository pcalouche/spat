import React, {useState} from 'react';
import {Link, NavLink as RRNavLink} from 'react-router-dom';
import {Container, Nav, Navbar} from 'react-bootstrap';

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
        <Container fluid>
          <Navbar.Brand>SPAT</Navbar.Brand>
        </Container>
      </Navbar>
    );
  } else {
    return (
      <Navbar collapseOnSelect aria-label="navigation"
              role="navigation"
              variant="dark"
              bg="primary"
              expand="lg"
              expanded={isOpen}>
        <Container fluid>
          <Navbar.Brand>SPAT</Navbar.Brand>
          <Navbar.Toggle aria-controls="responsive-navbar-nav" onClick={toggleNavBar}/>
          <Navbar.Collapse id="responsive-navbar-nav">
            <Nav className="me-auto">
              <Nav.Link to="/users" as={RRNavLink} onClick={toggleNavBar}>Users</Nav.Link>
              <Nav.Link to="/teams" as={RRNavLink} onClick={toggleNavBar}>Teams</Nav.Link>
            </Nav>
            <Nav>
              <Nav.Link to="#" as={Link} onClick={logoutHandler}>Logout | {currentUser.username}</Nav.Link>
            </Nav>
          </Navbar.Collapse>
        </Container>
      </Navbar>
    );
  }
};

export default AppBar;