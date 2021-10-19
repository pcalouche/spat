import React from 'react';
import {Button, Modal} from 'react-bootstrap';

type Props = {
  show: boolean
  title?: string
  confirmButtonText?: string
  confirmButtonColor?: string
  confirmCallback: (e: React.MouseEvent<HTMLButtonElement>) => void
  cancelButtonText?: string
  cancelButtonColor?: string
  cancelCallback: () => void
  children: React.ReactNode
}

const ConfirmationModal: React.FC<Props> = ({
                                              show,
                                              title,
                                              confirmButtonText = 'Ok',
                                              confirmButtonColor = 'success',
                                              confirmCallback,
                                              cancelButtonText = 'Cancel',
                                              cancelButtonColor = 'secondary',
                                              cancelCallback,
                                              children
                                            }) => {
  return (
    <Modal show={show}
           onHide={cancelCallback}
           backdrop="static">
      <Modal.Header closeButton>
        <Modal.Title>{title}</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        {children}
      </Modal.Body>
      <Modal.Footer>
        <Button variant={confirmButtonColor}
                className="mr-1"
                onClick={confirmCallback}>{confirmButtonText}
        </Button>
        <Button variant={cancelButtonColor}
                onClick={cancelCallback}>{cancelButtonText}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default ConfirmationModal;