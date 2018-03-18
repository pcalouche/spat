import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { UserSessionService } from '@core/services/user-session.service';
import { TeamRoutingModule } from '@features/team/team-routing.module';
import { NgbModal, NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { TeamService } from '@rest-services/api/team/team.service';
import { SharedModule } from '@shared/shared.module';
import { TeamListComponent } from './team-list.component';

describe('TeamListComponent', () => {
  let component: TeamListComponent;
  let fixture: ComponentFixture<TeamListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        SharedModule,
        TeamRoutingModule,
        NgbModule.forRoot()
      ],
      declarations: [
        TeamListComponent
      ],
      providers: [
        NgbModal,
        TeamService,
        UserSessionService
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
