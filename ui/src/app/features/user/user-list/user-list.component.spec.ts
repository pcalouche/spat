import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { UserSessionService } from '@core/services/user-session.service';
import { UserRoutingModule } from '@features/user/user-routing.module';
import { NgbModal, NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { UserService } from '@rest-services/api/user/user.service';
import { SharedModule } from '@shared/shared.module';
import { UserListComponent } from './user-list.component';

describe('UserListComponent', () => {
  let component: UserListComponent;
  let fixture: ComponentFixture<UserListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        SharedModule,
        UserRoutingModule,
        NgbModule.forRoot()
      ],
      declarations: [
        UserListComponent
      ],
      providers: [
        NgbModal,
        UserService,
        UserSessionService
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UserListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
