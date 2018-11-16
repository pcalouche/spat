import React                                                                               from 'react';
import PropTypes                                                                           from 'prop-types';
import {Button, Form, FormGroup, Input, Label, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';
import {Field, Formik}                                                                     from 'formik';
import * as Yup                                                                            from 'yup';

const UserModal = (props) => {
  let modalTitle = '';
  let buttonText = '';

  switch (props.mode) {
    case 'Add':
      modalTitle = 'Add User';
      buttonText = 'Save User';
      break;
    case 'Edit':
      modalTitle = 'Edit User';
      buttonText = 'Save User';
      break;
    case 'Delete':
      modalTitle = 'Delete User';
      buttonText = 'Delete User';
      break;
    default:
      break;
  }

  return (
    <Formik
      initialValues={{username: props.user.username ? props.user.username : ''}}
      enableReinitialize={true}
      validationSchema={Yup.object().shape({
        username: Yup.string().required()
      })}
      isInitialValid={(formikBag) => {
        return formikBag.validationSchema.isValidSync(formikBag.initialValues);
      }}
      onSubmit={async (values, actions) => {
        await props.submitCallback({...props.user, ...values});
        actions.setSubmitting(false);
      }}
      render={(formikProps) => (
        <Modal isOpen={props.open} toggle={props.cancelModal}>
          <ModalHeader>{modalTitle}</ModalHeader>
          <Form onSubmit={e => e.preventDefault()}>
            <ModalBody>
              {props.errorMessage &&
              <Label className="text-danger font-weight-bold">Error: {props.errorMessage}</Label>}
              {props.mode === 'Delete' &&
              <p>Are you sure you want to delete {props.user.username}?</p>
              }
              {props.mode !== 'Delete' &&
              <FormGroup>
                <Label>Username</Label>
                <Field
                  name="username"
                  render={(props) => <Input {...props.field} placeholder="Username"/>}
                />
              </FormGroup>
              }
            </ModalBody>
            <ModalFooter>
              <Button
                color="primary"
                onClick={formikProps.handleSubmit}
                disabled={!formikProps.isValid || formikProps.isSubmitting}>
                {buttonText}</Button>
              <Button
                color="primary"
                onClick={props.cancelCallback}>
                Cancel
              </Button>
            </ModalFooter>
          </Form>
        </Modal>
      )}
    />
  );
};

UserModal.propTypes = {
  open: PropTypes.bool.isRequired,
  mode: PropTypes.oneOf(['Add', 'Edit', 'Delete']).isRequired,
  user: PropTypes.object.isRequired,
  errorMessage: PropTypes.string,
  submitCallback: PropTypes.func.isRequired,
  cancelCallback: PropTypes.func.isRequired
};

export default UserModal;