import {HttpErrorResponse, HttpEvent, HttpHandler, HttpInterceptor, HttpRequest} from '@angular/common/http';
import {Injectable} from '@angular/core';
import 'rxjs/add/operator/do';
import {Observable} from 'rxjs/Observable';
import {TokenService} from '../services/token.service';

@Injectable()
export class CustomHttpInterceptor implements HttpInterceptor {

  constructor(private tokenService: TokenService) {
  }

  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    // If there is active token and the Authorization header does not have a value add it to the request.
    // Otherwise, send the request as is.  This takes care of cases where some requests don't require
    // authentication (e.g. token endpoint) and also cases where an endpoint may set the authentication
    // itself (e.g. refreshToken sets header value to refreshToken value)
    if (this.tokenService.getToken() && !req.headers.has(TokenService.AUTH_HTTP_HEADER)) {
      const authReq = req.clone({headers: req.headers.set(TokenService.AUTH_HTTP_HEADER, this.tokenService.getHeaderValue())});
      return next.handle(authReq).do(this.httpSuccessHandler, this.httpErrorHandler);
    } else {
      return next.handle(req).do(this.httpSuccessHandler, this.httpErrorHandler);
    }
  }

  httpSuccessHandler() {
    // Empty function for now, but it could be used to do some custom logic for all HTTP requests
  };

  httpErrorHandler(error) {
    // Maybe consider displaying a modal in certain cases to the user, but for now just log the error.
    if (error instanceof HttpErrorResponse) {
      console.error(error);
      console.error(`message: ${error.message}`);
      console.error(`error: ${error.error}`);
    }
  };
}
