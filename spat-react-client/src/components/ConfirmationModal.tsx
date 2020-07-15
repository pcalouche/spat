import React from 'react';
import {Button, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';

type Props = {
  isOpen: boolean
  title?: string
  confirmButtonText?: string
  confirmButtonColor?: string
  confirmCallback: (e: React.MouseEvent<HTMLButtonElement>) => void
  cancelButtonText?: string
  cancelButtonColor?: string
  cancelCallback: (e: React.MouseEvent<HTMLButtonElement>) => void
  children: React.ReactNode
}

const ConfirmationModal: React.FC<Props> = ({
                                              isOpen,
                                              title,
                                              confirmButtonText = 'Ok',
                                              confirmButtonColor = 'success',
                                              confirmCallback,
                                              cancelButtonText = 'Cancel',
                                              cancelButtonColor = 'secondary',
                                              cancelCallback,
                                              children
                                            }) =>
  (
    <Modal isOpen={isOpen}
           backdrop="static">
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

export default React.memo(ConfirmationModal);