import { CommonModule } from '@angular/common';
import { HttpClientModule } from '@angular/common/http';
import { ModuleWithProviders, NgModule } from '@angular/core';
import { ReactiveFormsModule } from '@angular/forms';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';

@NgModule({
  imports: [
    // Angular stuff
    CommonModule,
    HttpClientModule,
    ReactiveFormsModule,
    // Third party stuff
    NgbModule
    // Stuff from shared module
  ],
  declarations: [],
  exports: [
    // Angular stuff
    CommonModule,
    HttpClientModule,
    ReactiveFormsModule,
    // Third party stuff
    NgbModule
    // Stuff from shared module
  ]
})
export class SharedModule {
  static forRoot(): ModuleWithProviders {
    return {
      ngModule: SharedModule,
      providers: [] // For services in the shared module
    };
  }
}
