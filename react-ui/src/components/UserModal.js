import React                                                from 'react';
import PropTypes                                            from 'prop-types';
import {Button, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';

const UserModal = (props) => {
  let modalTitle = '';

  switch (props.mode) {
    case 'Add':
      modalTitle = 'Add User';
      break;
    case 'Edit':
      modalTitle = 'Edit User';
      break;
    case 'Delete':
      modalTitle = 'Delete User';
      break;
  }
  return (

    <Modal isOpen={props.open} toggle={props.cancelModal}>
      <ModalHeader>{modalTitle}</ModalHeader>
      <ModalBody>
        Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
      </ModalBody>
      <ModalFooter>
        <Button color="primary" onClick={props.cancelModal}>Save Team</Button>
        <Button color="primary" onClick={props.cancelModal}>Cancel</Button>
      </ModalFooter>
    </Modal>
  );
};

UserModal.propTypes = {
  open: PropTypes.bool.isRequired,
  title: PropTypes.string.isRequired,
  user: PropTypes.object.isRequired
};

export default UserModal;