import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TeamManagerModalComponent } from './team-manager-modal.component';

describe('TeamManagerModalComponent', () => {
  let component: TeamManagerModalComponent;
  let fixture: ComponentFixture<TeamManagerModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TeamManagerModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamManagerModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
