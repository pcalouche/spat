import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { NavComponent } from '@core/components/nav/nav.component';
import { UserSessionService } from '@core/services/user-session.service';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { AuthService } from '@rest-services/api/auth/auth.service';
import { AppComponent } from './app.component';

describe('AppComponent', () => {
  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        HttpClientTestingModule,
        RouterTestingModule,
        NgbModule.forRoot()
      ],
      declarations: [
        AppComponent,
        NavComponent
      ],
      providers: [
        UserSessionService,
        AuthService
      ]
    }).compileComponents();
  }));

  it('should create the app', async(() => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app).toBeTruthy();
  }));

});
