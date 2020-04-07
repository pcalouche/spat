import React, {useState} from 'react';

import AppContext from './AppContext';

const AppProvider = (props) => {
  const [currentUser, setCurrentUser] = useState(null);

  const value = {
    currentUser,
    setCurrentUser,
    isAdmin: currentUser && currentUser.roles.filter(role => role.name === 'Admin').length !== 0
  };

  return (
    <AppContext.Provider value={value}>
      {props.children}
    </AppContext.Provider>
  );
};

export default AppProvider;