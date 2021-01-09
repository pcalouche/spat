import React, {useState} from 'react';
import {Button, Form, Modal} from 'react-bootstrap';
import {Field, FieldProps, Formik, FormikHelpers} from 'formik';
import * as Yup from 'yup';

import {teamApi} from '../api';
import {Team} from '../types';

type Props = {
  show: boolean
  mode: 'Add' | 'Edit'
  team: Team
  submitCallback: () => Promise<void>
  cancelCallback: (e: React.MouseEvent<HTMLButtonElement>) => void
}

type TeamFormFields = {
  name?: string
}

const TeamModal: React.FC<Props> = ({
                                      show,
                                      mode = 'Add',
                                      team,
                                      submitCallback,
                                      cancelCallback
                                    }) => {
  const [errorMessage, setErrorMessage] = useState(undefined);

  const form: {initialValues: TeamFormFields, validationSchema: Yup.AnyObjectSchema} = {
    initialValues: {
      name: team?.name || ''
    },
    validationSchema: Yup.object().shape({
      name: Yup.string().required('Required')
    })
  };

  const handleSubmit = async (formikValues: TeamFormFields, formikHelpers: FormikHelpers<TeamFormFields>) => {
    const teamRequest = {...formikValues};

    try {
      if (mode === 'Add') {
        await teamApi.createTeam(teamRequest);
      } else {
        await teamApi.updateTeam(team.id, teamRequest);
      }
      formikHelpers.setSubmitting(false);
      await submitCallback();
    } catch (error) {
      formikHelpers.setSubmitting(false);
      setErrorMessage(error.message);
    }
  };

  return (
    <Formik initialValues={form.initialValues}
            validationSchema={form.validationSchema}
            enableReinitialize={true}
            onSubmit={handleSubmit}>
      {(formikProps) => (
        <Modal show={show}
               onHide={cancelCallback}
               backdrop="static">
          <Modal.Header closeButton>
            <Modal.Title>
              {mode === 'Add' ? 'Add Team' : 'Edit Team'}
            </Modal.Title>
          </Modal.Header>
          <Form onSubmit={formikProps.handleSubmit}>
            <Modal.Body>
              {errorMessage &&
              <h6 className="text-danger">{errorMessage}</h6>
              }
              <Form.Group controlId="name">
                <Form.Label>Name</Form.Label>
                <Field name="name">
                  {({field, meta}: FieldProps) => (
                    <Form.Control {...field}
                                  placeholder="Name"
                                  isInvalid={!!(meta.touched && meta.error)}/>
                  )}
                </Field>
                <Form.Control.Feedback type="invalid">
                  {formikProps.errors.name}
                </Form.Control.Feedback>
              </Form.Group>
            </Modal.Body>
            <Modal.Footer>
              <Button variant="success"
                      onClick={formikProps.submitForm}
                      disabled={!formikProps.dirty || !formikProps.isValid || formikProps.isSubmitting}>
                {mode === 'Add' ? 'Add Team' : 'Save Team'}
              </Button>
              <Button variant="secondary" onClick={cancelCallback}>
                Cancel
              </Button>
            </Modal.Footer>
          </Form>
        </Modal>
      )}
    </Formik>
  );
};

export default TeamModal;