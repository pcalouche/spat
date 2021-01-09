import React, {useState} from 'react';
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
import {Field, FieldProps, Formik, FormikHelpers} from 'formik';
import * as Yup from 'yup';

import {teamApi} from '../api';
import {Team} from '../types';

type Props = {
  isOpen: boolean
  mode: 'Add' | 'Edit'
  team: Team
  submitCallback: () => Promise<void>
  cancelCallback: (e: React.MouseEvent<HTMLButtonElement>) => void
}

type TeamFormFields = {
  name?: string
}

const TeamModal: React.FC<Props> = ({
                                      isOpen,
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
                  {({field, meta}: FieldProps) => (
                    <Input {...field}
                           placeholder="Name"
                           invalid={!!(meta.touched && meta.error)}/>
                  )}
                </Field>
                <FormFeedback>{formikProps.errors.name}</FormFeedback>
              </FormGroup>
            </ModalBody>
            <ModalFooter>
              <Button color="primary"
                      onClick={formikProps.submitForm}
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

export default TeamModal;