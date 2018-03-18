import { inject, TestBed } from '@angular/core/testing';
import { UserSessionService } from '@core/services/user-session.service';

import { HttpInterceptorService } from './http-interceptor.service';

describe('HttpInterceptorService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        HttpInterceptorService,
        UserSessionService
      ]
    });
  });

  it('should be created', inject([HttpInterceptorService], (service: HttpInterceptorService) => {
    expect(service).toBeTruthy();
  }));
});
