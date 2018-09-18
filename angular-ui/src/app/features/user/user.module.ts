import {NgModule}           from '@angular/core';
import {SharedModule}       from '@shared/shared.module';
import {UserListComponent}  from './user-list/user-list.component';
import {UserModalComponent} from './user-list/user-modal/user-modal.component';
import {UserRoutingModule}  from './user-routing.module';

@NgModule({
  imports: [
    SharedModule,
    UserRoutingModule
  ],
  declarations: [UserListComponent, UserModalComponent],
  entryComponents: [UserModalComponent]
})
export class UserModule {
}
