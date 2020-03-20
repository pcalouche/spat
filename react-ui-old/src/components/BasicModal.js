import React                                                from 'react';
import PropTypes                                            from 'prop-types';
import {Button, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';

const BasicModal = props => {
  return (
    <Modal isOpen={props.open} toggle={props.cancelModal}>
      <ModalHeader>{props.title}</ModalHeader>
      <ModalBody>{props.message}</ModalBody>
      <ModalFooter>
        <Button
          color="primary"
          onClick={props.submitCallback}>
          Ok</Button>
        {props.showCancelButton &&
        <Button
          color="primary"
          onClick={props.cancelCallback}>
          Cancel
        </Button>
        }
      </ModalFooter>
    </Modal>
  );
};

BasicModal.defaultProps = {
  showCancelButton: true,
  cancelCallback: () => true
};

BasicModal.propTypes = {
  open: PropTypes.bool.isRequired,
  title: PropTypes.string.isRequired,
  message: PropTypes.string.isRequired,
  submitCallback: PropTypes.func.isRequired,
  cancelCallback: PropTypes.func.isRequired,
  showCancelButton: PropTypes.bool.isRequired
};


export default BasicModal;