import { User } from '@app/rest/api/model/user.model';

export class ClientUser {
  private id: number;
  private username: string;
  private authorities: any[] = [];

  constructor(user: User) {
    this.id = user.id;
    this.username = user.username;
    this.authorities = user.authorities;
    // for (let i = 0; i < user.authorities.length; i++) {
    //   this.authorities.push(user.authorities[i].authority);
    // }
  }

  hasAuthority(authorityValue: string): boolean {
    for (let i = 0; i < this.authorities.length; i++) {
      if (String(this.authorities[i].authority) === authorityValue) {
        return true;
      }
    }
    return false;
  }
}
