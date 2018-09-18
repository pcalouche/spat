import {User} from '@rest-services/api/model/user.model';

export class ClientUser {
  id: number;
  username: string;
  roles: [{id: number, name: string}];

  constructor(user: User) {
    this.id = user.id;
    this.username = user.username;
    this.roles = user.roles;
  }

  hasRole(role: string): boolean {
    let hasRole = false;
    for (let i = 0; i < this.roles.length; i++) {
      if (this.roles[i].name === role) {
        hasRole = true;
        break;
      }
    }
    return hasRole;
  }
}
