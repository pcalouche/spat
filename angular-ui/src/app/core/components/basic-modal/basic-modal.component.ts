import {Component, Input, OnInit} from '@angular/core';
import {NgbActiveModal}           from '@ng-bootstrap/ng-bootstrap';

export interface BasicModalConfig {
  type: string;
  title: string;
  message: string;
}

/**
 * Provide some HTML with an action to open the modal
 * <button class="btn btn-primary" (click)="openModal()">Launch Info Modal</button>
 *
 * The component that calls the modal should have an instance of NgbModal
 * constructor(private modalService: NgbModal) {
 * }
 *
 * // Provide a valid @BasicModalConfig to the @BasicModalComponent
 * openModal() {
 *   const modalRef = this.modalService.open(BasicModalComponent);
 *   modalRef.componentInstance.config = {type: 'info', title: 'My Modal Title', message: 'My awesome message!'};
 * }
 */
@Component({
  selector: 'app-basic-modal',
  templateUrl: './basic-modal.component.html',
  styleUrls: ['./basic-modal.component.scss']
})
export class BasicModalComponent implements OnInit {
  @Input() config: BasicModalConfig;
  type: string;
  title: string;
  message: string;
  iconClass: string;
  showCancel = false;

  constructor(public activeModal: NgbActiveModal) {

  }

  ngOnInit() {
    this.type = this.config.type;
    this.title = this.config.title;
    this.message = this.config.message;
    switch (this.type) {
      case 'info':
        this.iconClass = 'fa-info-circle info';
        break;
      case 'confirm':
        this.iconClass = 'fa-question-circle confirm';
        this.showCancel = true;
        break;
      case 'warning':
        this.iconClass = 'fa-exclamation-circle warning';
        break;
      case 'error':
        this.iconClass = 'fa-exclamation-circle error';
        break;
      default:
        throw new Error(this.type + ' is not a valid basic modal type');
    }
  }
}
