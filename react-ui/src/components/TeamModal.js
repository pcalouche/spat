import React                                                                               from 'react';
import PropTypes                                                                           from 'prop-types';
import {Button, Form, FormGroup, Input, Label, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';
import {Field, Formik}                                                                     from 'formik';
import * as Yup                                                                            from 'yup';

const TeamModal = props => {
  let modalTitle = '';
  let buttonText = '';

  switch (props.mode) {
    case 'Add':
      modalTitle = 'Add Team';
      buttonText = 'Save Team';
      break;
    case 'Edit':
      modalTitle = 'Edit Team';
      buttonText = 'Save Team';
      break;
    case 'Delete':
      modalTitle = 'Delete Team';
      buttonText = 'Delete Team';
      break;
    default:
      break;
  }

  return (
    <Formik
      initialValues={{name: props.team.name ? props.team.name : ''}}
      enableReinitialize={true}
      validationSchema={Yup.object().shape({
        name: Yup.string().required()
      })}
      isInitialValid={(formikBag) => {
        return formikBag.validationSchema.isValidSync(formikBag.initialValues);
      }}
      onSubmit={async (values, actions) => {
        await props.submitCallback({...props.team, ...values});
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
              <p>Are you sure you want to delete {props.team.name}?</p>
              }
              {props.mode !== 'Delete' &&
              <FormGroup>
                <Label>Name</Label>
                <Field
                  name="name"
                  render={(props) => <Input {...props.field} placeholder="Team Name"/>}
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

TeamModal.propTypes = {
  open: PropTypes.bool.isRequired,
  mode: PropTypes.oneOf(['Add', 'Edit', 'Delete']).isRequired,
  team: PropTypes.object.isRequired,
  errorMessage: PropTypes.string,
  submitCallback: PropTypes.func.isRequired,
  cancelCallback: PropTypes.func.isRequired
};

export default TeamModal;