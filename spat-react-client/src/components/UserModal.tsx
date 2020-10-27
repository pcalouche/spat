import React, {useState} from 'react';
import {Button, Form, Modal} from 'react-bootstrap';
import {Field, FieldProps, Formik, FormikHelpers} from 'formik';
import * as Yup from 'yup';

import {userApi} from '../api';
import {User} from '../types';

type Props = {
  show: boolean,
  mode: 'Add' | 'Edit',
  user: User
  submitCallback: () => Promise<void>,
  cancelCallback: (e: React.MouseEvent<HTMLButtonElement>) => void
}

type UserFormFields = {
  username: string
  isAdmin: boolean
  enabled: boolean
  accountLocked: boolean
  credentialsExpired: boolean
  accountExpired: boolean
}

const UserModal: React.FC<Props> = ({
                                      show,
                                      mode = 'Add',
                                      user,
                                      submitCallback,
                                      cancelCallback
                                    }) => {
  const [errorMessage, setErrorMessage] = useState(undefined);

  const form: {initialValues: UserFormFields, validationSchema: Yup.ObjectSchema} = {
    initialValues: {
      username: user.username,
      isAdmin: user.roles.filter(role => role.name === 'Admin').length === 1,
      enabled: user.enabled,
      accountLocked: !user.accountNonLocked,
      credentialsExpired: !user.credentialsNonExpired,
      accountExpired: !user.accountNonExpired
    },
    validationSchema: Yup.object().shape({
      username: Yup.string().required('Required'),
      isAdmin: Yup.bool(),
      enabled: Yup.bool(),
      accountLocked: Yup.bool(),
      credentialsExpired: Yup.bool(),
      accountExpired: Yup.bool()
    })
  };

  const handleSubmit = async (formikValues: UserFormFields, formikActions: FormikHelpers<UserFormFields>) => {
    const userRequest = {
      username: formikValues.username,
      roles: formikValues.isAdmin ? [{name: 'Admin'}] : [],
      enabled: formikValues.enabled,
      accountNonLocked: !formikValues.accountLocked,
      credentialsNonExpired: !formikValues.credentialsExpired,
      accountNonExpired: !formikValues.accountExpired
    };

    try {
      if (mode === 'Add') {
        await userApi.createUser(userRequest);
      } else {
        await userApi.updateUser(user.id, userRequest);
      }
      formikActions.setSubmitting(false);
      await submitCallback();
    } catch (error) {
      formikActions.setSubmitting(false);
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
              {mode === 'Add' ? 'Add User' : 'Edit User'}
            </Modal.Title>
          </Modal.Header>
          <Form onSubmit={formikProps.handleSubmit}>
            <Modal.Body>
              {errorMessage &&
              <h6 className="text-danger">{errorMessage}</h6>
              }
              <Form.Group controlId="username">
                <Form.Label>Username</Form.Label>
                <Field name="username">
                  {({field, meta}: FieldProps) => (
                    <Form.Control {...field}
                                  placeholder="Username"
                                  isInvalid={!!(meta.touched && meta.error)}/>
                  )}
                </Field>
                <Form.Control.Feedback type="invalid">
                  {formikProps.errors.username}
                </Form.Control.Feedback>
              </Form.Group>
              <Form.Group>
                <Form.Label>Roles</Form.Label>
                <Field name="isAdmin">
                  {({field}: FieldProps) => (
                    <Form.Check {...field}
                                type="switch"
                                id="isAdminSwitch"
                                label="Admin"
                                checked={field.value}/>
                  )}
                </Field>
              </Form.Group>
              <Form.Group>
                <Form.Label>User Status</Form.Label>
                <Field name="enabled">
                  {({field}: FieldProps) => (
                    <Form.Check {...field}
                                type="switch"
                                id="enabledSwitch"
                                label="Enabled"
                                checked={field.value}/>
                  )}
                </Field>
                <Field name="accountLocked">
                  {({field}: FieldProps) => (
                    <Form.Check {...field}
                                type="switch"
                                id="accountLockedSwitch"
                                label="Locked"
                                checked={field.value}/>
                  )}
                </Field>
                <Field name="credentialsExpired">
                  {({field}: FieldProps) => (
                    <Form.Check {...field}
                                type="switch"
                                id="credentialsExpiredSwitch"
                                label="Credentials Expired"
                                checked={field.value}/>
                  )}
                </Field>
                <Field name="accountExpired">
                  {({field}: FieldProps) => (
                    <Form.Check {...field}
                                type="switch"
                                id="accountExpiredSwitch"
                                label="Expired"
                                checked={field.value}/>
                  )}
                </Field>
              </Form.Group>
            </Modal.Body>
            <Modal.Footer>
              <Button variant="success"
                      onClick={formikProps.submitForm}
                      disabled={!formikProps.dirty || !formikProps.isValid || formikProps.isSubmitting}>
                {mode === 'Add' ? 'Add User' : 'Save User'}
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

export default UserModal;