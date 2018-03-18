import { HttpClientTestingModule } from '@angular/common/http/testing';
import { inject, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { UserSessionService } from '@core/services/user-session.service';

import { RouteGuardService } from './route-guard.service';

describe('RouteGuardService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        RouterTestingModule
      ],
      providers: [
        RouteGuardService,
        UserSessionService
      ]
    });
  });

  it('should be created', inject([RouteGuardService], (service: RouteGuardService) => {
    expect(service).toBeTruthy();
  }));
});
