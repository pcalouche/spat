import {NgModule}             from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {UserListComponent}    from '@features/user/user-list/user-list.component';

const routes: Routes = [{
  path: '',
  component: UserListComponent,
  children: []
}];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class UserRoutingModule {
}
