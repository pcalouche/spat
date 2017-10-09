import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TeamManagerComponent } from './team-manager.component';

describe('TeamManagerComponent', () => {
  let component: TeamManagerComponent;
  let fixture: ComponentFixture<TeamManagerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TeamManagerComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamManagerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
