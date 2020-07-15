import React, {useState} from 'react';
import {User} from '../types';

import {AppContext, AppContextType} from './AppContext';

type Props = {
  children: React.ReactNode
}

const AppProvider: React.FC<Props> = ({children}) => {
  const [currentUser, setCurrentUser] = useState<User | null>(null);

  const value: AppContextType = {
    currentUser,
    setCurrentUser,
    isAdmin: currentUser ? currentUser.roles.filter(role => role.name === 'Admin').length !== 0 : false
  };

  return (
    <AppContext.Provider value={value}>
      {children}
    </AppContext.Provider>
  );
};

export default AppProvider;