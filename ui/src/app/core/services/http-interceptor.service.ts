import { HttpEvent, HttpHandler, HttpInterceptor, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { RestServiceHelper } from '@app/rest/api/rest-service-helper';
import { SessionManagementService } from '@core/services/session-management.service';
import { Observable } from 'rxjs/Observable';

@Injectable()
export class HttpInterceptorService implements HttpInterceptor {

  constructor(private sessionManagementService: SessionManagementService) {
  }

  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    if (!req.headers.has(RestServiceHelper.authHttpHeader)) {
      const newReq = req.clone({
        headers: req.headers.set(
          RestServiceHelper.authHttpHeader,
          RestServiceHelper.authHeaderBearerPrefix + this.sessionManagementService.getToken()
        )
      });
      return next.handle(newReq);
    } else {
      return next.handle(req);
    }
  }
}
