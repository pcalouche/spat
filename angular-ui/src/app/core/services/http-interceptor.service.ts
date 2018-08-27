import {HttpEvent, HttpHandler, HttpInterceptor, HttpRequest} from '@angular/common/http';
import {Injectable}                                           from '@angular/core';
import {UserSessionService}                                   from '@core/services/user-session.service';
import {RestServiceHelper}                                    from '@rest-services/api/rest-service-helper';
import {Observable}                                           from 'rxjs';

@Injectable()
export class HttpInterceptorService implements HttpInterceptor {

  constructor(private userSessionService: UserSessionService) {
  }

  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    if (!req.headers.has(RestServiceHelper.authHttpHeader)) {
      const newReq = req.clone({
        headers: req.headers.set(
          RestServiceHelper.authHttpHeader,
          RestServiceHelper.authHeaderBearerPrefix + this.userSessionService.getToken()
        )
      });
      return next.handle(newReq);
    } else {
      return next.handle(req);
    }
  }
}
