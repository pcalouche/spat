import { TestBed, inject } from '@angular/core/testing';

import { SessionManagementService } from './session-management.service';

describe('SessionManagementService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [SessionManagementService]
    });
  });

  it('should be created', inject([SessionManagementService], (service: SessionManagementService) => {
    expect(service).toBeTruthy();
  }));
});
