import { User } from '@rest-services/api/model/user.model';

export class ClientUser {
  id: number;
  username: string;
  authorities: string[] = [];

  constructor(user: User) {
    this.id = user.id;
    this.username = user.username;
    this.authorities = user.authorities;
  }

  hasAuthority(authority: string): boolean {
    return this.authorities.indexOf(authority) !== -1;
  }
}
