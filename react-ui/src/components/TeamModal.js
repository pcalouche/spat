import React                                                                               from 'react';
import PropTypes                                                                           from 'prop-types';
import {Button, Form, FormGroup, Input, Label, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';
import {Field, Formik}                                                                     from 'formik';
import * as Yup                                                                            from 'yup';

const TeamModal = (props) => {
  console.info(props);
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
  }

  return (
    <Modal isOpen={props.open} toggle={props.cancelModal}>
      <ModalHeader>{modalTitle}</ModalHeader>
      <ModalBody>
        <Formik
          initialValues={{name: props.team.name}}
          validationSchema={Yup.object().shape({
            name: Yup.string().required()
          })}
          render={() => (
            <Form>
              {props.errorMessage !== '' && <Label>Error: {props.errorMessage}</Label>}
              <FormGroup>
                <Label>Name</Label>
                <Field
                  name="name"
                  render={(props) => <Input type="name" {...props.field} placeholder="Team Name"/>}
                />
              </FormGroup>
            </Form>
          )}
        />
      </ModalBody>
      <ModalFooter>
        <Button color="primary" onClick={() => props.callback(props.team)}>{buttonText}</Button>
        <Button color="primary" onClick={props.cancelModal}>Cancel</Button>
      </ModalFooter>
    </Modal>
  );
};

TeamModal.propTypes = {
  open: PropTypes.bool.isRequired,
  mode: PropTypes.string.isRequired,
  team: PropTypes.object.isRequired,
  errorMessage: PropTypes.string,
  callback: PropTypes.func.isRequired
};

export default TeamModal;