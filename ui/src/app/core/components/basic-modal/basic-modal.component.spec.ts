import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { NgbActiveModal, NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { BasicModalComponent } from './basic-modal.component';

describe('BasicModalComponent', () => {
  let component: BasicModalComponent;
  let fixture: ComponentFixture<BasicModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        NgbModule.forRoot()
      ],
      declarations: [BasicModalComponent],
      providers: [
        NgbActiveModal
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BasicModalComponent);
    component = fixture.componentInstance;
    component.config = {
      type: 'info',
      title: 'hello',
      message: 'world'
    };
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
