import { User } from '@rest-services/api/model/user.model';

export class ClientUser {
  id: number;
  username: string;
  roles: string[] = [];

  constructor(user: User) {
    this.id = user.id;
    this.username = user.username;
    console.info(user.roles);
    this.roles = user.roles.map(item => item.name);
  }

  hasRole(role: string): boolean {
    return this.roles.indexOf(role) !== -1;
  }
}
