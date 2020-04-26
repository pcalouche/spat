import React, {useState} from 'react';
import PropTypes from 'prop-types';
import {
  Button,
  Form,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Modal,
  ModalBody,
  ModalFooter,
  ModalHeader
} from 'reactstrap';
import {Field, Formik} from 'formik';
import * as Yup from 'yup';

import {teamApi} from '../api';

const TeamModal = ({isOpen, mode, team, submitCallback, cancelCallback}) => {
  const [errorMessage, setErrorMessage] = useState(undefined);

  const initialValues = {
    name: team.name
  };

  const validationSchema = Yup.object().shape({
    name: Yup.string().required('Required')
  });

  const handleSubmit = async (formikValues, formikActions) => {
    const teamRequest = {...formikValues};

    try {
      if (mode === 'Add') {
        await teamApi.createTeam(teamRequest);
      } else {
        await teamApi.updateTeam(team.id, teamRequest);
      }
      formikActions.setSubmitting(false);
      submitCallback(formikValues);
    } catch (error) {
      formikActions.setSubmitting(false);
      setErrorMessage(error.message);
    }
  };

  return (
    <Formik initialValues={initialValues}
            validationSchema={validationSchema}
            onSubmit={handleSubmit}>
      {(formikProps) => (
        <Modal isOpen={isOpen}
               backdrop="static">
          <ModalHeader>{mode === 'Add' ? 'Add Team' : 'Edit Team'}</ModalHeader>
          <Form onSubmit={formikProps.handleSubmit}>
            <ModalBody>
              {errorMessage &&
              <h6 className="text-danger">{errorMessage}</h6>
              }
              <FormGroup>
                <Label>Username</Label>
                <Field name="name">
                  {({field, meta}) => (
                    <Input {...field}
                           placeholder="Name"
                           invalid={!!(meta.touched && meta.error)}/>
                  )}
                </Field>
                <FormFeedback>{formikProps.errors.username}</FormFeedback>
              </FormGroup>
            </ModalBody>
            <ModalFooter>
              <Button color="primary"
                      onClick={formikProps.handleSubmit}
                      disabled={!formikProps.dirty || !formikProps.isValid || formikProps.isSubmitting}>
                {mode === 'Add' ? 'Add Team' : 'Save Team'}
              </Button>
              <Button color="secondary" onClick={cancelCallback}>
                Cancel
              </Button>
            </ModalFooter>
          </Form>
        </Modal>
      )}
    </Formik>
  );
};

TeamModal.propTypes = {
  isOpen: PropTypes.bool.isRequired,
  mode: PropTypes.oneOf(['Add', 'Edit']).isRequired,
  team: PropTypes.object.isRequired,
  submitCallback: PropTypes.func.isRequired,
  cancelCallback: PropTypes.func.isRequired
};

TeamModal.defaultProps = {
  mode: 'Add'
};

export default TeamModal;