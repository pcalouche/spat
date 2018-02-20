import { HttpErrorResponse, HttpEvent, HttpHandler, HttpInterceptor, HttpRequest } from '@angular/common/http';
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
      console.log('adding header');
      const newReq = req.clone({
        headers: req.headers.set(
          RestServiceHelper.authHttpHeader,
          RestServiceHelper.authHeaderBearerPrefix + this.sessionManagementService.getToken()
        )
      });
      return next.handle(newReq);
      // return next.handle(newReq).pipe(tap(this.httpSuccessHandler, this.httpErrorHandler));
    } else {
      console.log('not adding header');
      return next.handle(req);
      // return next.handle(req).pipe(tap(this.httpSuccessHandler, this.httpErrorHandler));
    }
  }

  httpSuccessHandler() {
    // Empty function for now, but it could be used to do some custom logic for all HTTP requests
  }

  httpErrorHandler(error) {
    // Maybe consider displaying a modal in certain cases to the user, but for now just log the error.
    if (error instanceof HttpErrorResponse) {
      console.error(error);
      console.error(`message: ${error.message}`);
      console.error(`error: ${error.error}`);
    }
  }
}
