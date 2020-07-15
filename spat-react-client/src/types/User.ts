import Role from './Role';

type User = {
  id: number
  username: string
  roles: Role[]
  accountNonExpired: boolean
  accountNonLocked: boolean
  credentialsNonExpired: boolean
  enabled: boolean
}

export default User;