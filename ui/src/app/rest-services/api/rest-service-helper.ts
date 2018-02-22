export class RestServiceHelper {
  public static readonly apiRoot = 'http://localhost:10000/spat/rest-services/api';
  public static readonly authHeaderBasicPrefix = 'Basic ';
  public static readonly authHeaderBearerPrefix = 'Bearer ';
  public static readonly authHttpHeader = 'Authorization';
  public static readonly clientCodes = {
    badCredentials: 'BAD_CREDENTIALS',
    usernameNotFound: 'USERNAME_NOT_FOUND',
    accountExpired: 'ACCOUNT_EXPIRED',
    accountCredentialsExpired: 'ACCOUNT_CREDENTIALS_EXPIRED',
    accountLocked: 'ACCOUNT_LOCKED',
    accountDisabled: 'ACCOUNT_DISABLED',
    expiredToken: 'EXPIRED_TOKEN',
    invalidToken: 'INVALID_TOKEN'
  };
}
