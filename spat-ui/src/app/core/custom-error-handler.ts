import {ErrorHandler} from '@angular/core';

export class CustomErrorHandler implements ErrorHandler {
  // This could be enhanced to do more down the road
  handleError(error) {
    console.error('An uncaught front end exception occurred! Details are below');
    console.error(error);
  }
}
