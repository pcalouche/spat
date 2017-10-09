import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { UserManagerModalComponent } from './user-manager-modal.component';

describe('UserManagerModalComponent', () => {
  let component: UserManagerModalComponent;
  let fixture: ComponentFixture<UserManagerModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ UserManagerModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UserManagerModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
