import React from 'react';

const AppContext = React.createContext({
  currentUser: undefined,
  setCurrentUser: user => {
  },
  isAdmin: false
});

export default AppContext;