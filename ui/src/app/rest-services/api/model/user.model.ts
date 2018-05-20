export interface User {
  id: number;
  username: string;
  accountNonExpired: boolean;
  accountNonLocked: boolean;
  credentialsNonExpired: boolean;
  enabled: boolean;
  roles: [{ id: number, name: string }];
}
