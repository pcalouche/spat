import React from 'react';
import PropTypes from 'prop-types';
import {Button, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';

const ConfirmationModal = ({
                             isOpen, title, confirmButtonText, confirmButtonColor, confirmCallback,
                             cancelButtonText, cancelButtonColor, cancelCallback, children, ...reactModalProps
                           }) =>
  (
    <Modal isOpen={isOpen}
           backdrop="static"
           {...reactModalProps}>
      <ModalHeader>{title}</ModalHeader>
      <ModalBody>
        {children}
      </ModalBody>
      <ModalFooter>
        <Button color={confirmButtonColor} className="mr-1" onClick={confirmCallback}>{confirmButtonText}</Button>
        <Button color={cancelButtonColor} onClick={cancelCallback}>{cancelButtonText}</Button>
      </ModalFooter>
    </Modal>
  );

ConfirmationModal.defaultProps = {
  isOpen: false,
  title: '',
  confirmButtonText: 'Ok',
  confirmButtonColor: 'success',
  cancelButtonText: 'Cancel',
  cancelButtonColor: 'secondary'
};

ConfirmationModal.propTypes = {
  isOpen: PropTypes.bool.isRequired,
  title: PropTypes.string.isRequired,
  confirmButtonText: PropTypes.string,
  confirmButtonColor: PropTypes.string,
  confirmCallback: PropTypes.func.isRequired,
  cancelButtonText: PropTypes.string,
  cancelButtonColor: PropTypes.string,
  cancelCallback: PropTypes.func.isRequired,
  children: PropTypes.node.isRequired
};

export default React.memo(ConfirmationModal);