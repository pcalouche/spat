import {inject, TestBed} from '@angular/core/testing';

import {CustomHttpInterceptor} from './custom-http.interceptor';
import {TokenService} from '../services/token.service';

describe('CustomHttpInterceptor', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CustomHttpInterceptor, TokenService]
    });
  });

  it('should be created', inject([CustomHttpInterceptor], (service: CustomHttpInterceptor) => {
    expect(service).toBeTruthy();
  }));
});
