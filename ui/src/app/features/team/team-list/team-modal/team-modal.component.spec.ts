import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { NgbActiveModal, NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { TeamService } from '../../../../rest-services/api/team/team.service';
import { TeamModalComponent } from './team-modal.component';

describe('TeamModalComponent', () => {
  let component: TeamModalComponent;
  let fixture: ComponentFixture<TeamModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        HttpClientTestingModule,
        NgbModule.forRoot()
      ],
      declarations: [
        TeamModalComponent
      ],
      providers: [
        NgbActiveModal,
        TeamService
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
