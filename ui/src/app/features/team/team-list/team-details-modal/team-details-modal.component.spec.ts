import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TeamDetailsModalComponent } from './team-details-modal.component';

describe('TeamDetailsModalComponent', () => {
  let component: TeamDetailsModalComponent;
  let fixture: ComponentFixture<TeamDetailsModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TeamDetailsModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamDetailsModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
