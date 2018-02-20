// Messages that are related to the core module and any of its components, directives, or pipes
// Good examples of what should go here are disclaimer and tooltip messages
export const coreModuleMessages = {
  // General messages
  // Component messages
  loginComponent: {},
  topNavComponent: {
    logoutWarning: 'You will be logged out shortly from inactivity.  Click OK to stay logged in.',
    loggedOut: 'You were logged out due to inactivity.'
  }
};
// Error messages that are related to the core module and any of its components, directives, or pipes
export const coreModuleErrors = {
  // General errors
  requestFailed: 'Unable to process request.',
  unknown: 'Unknown Error',
  // Component errors
  loginComponent: {
    badCredentials: 'Invalid username/password',
    accountExpired: 'Account is expired',
    accountLocked: 'Account is locked',
    accountCredentialsExpired: 'Account credentials are expired',
    accountDisabled: 'Account is disabled'
  }
};
