import React from 'react';

import {User} from '../types';

export type AppContextType = {
  currentUser: User | null,
  setCurrentUser: (user: User | null) => void,
  isAdmin: boolean
}

export const AppContext = React.createContext<AppContextType>({
  currentUser: null,
  setCurrentUser: (user) => user,
  isAdmin: false
});