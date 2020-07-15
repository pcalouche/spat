import React, {useState} from 'react';
import {
  Button,
  CustomInput,
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

import {userApi} from '../api';
import {User} from '../types';

type Props = {
  isOpen: boolean,
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
                                      isOpen,
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
            onSubmit={handleSubmit}>
      {(formikProps) => (
        <Modal isOpen={isOpen}
               backdrop="static">
          <ModalHeader>{mode === 'Add' ? 'Add User' : 'Edit User'}</ModalHeader>
          <Form onSubmit={formikProps.handleSubmit}>
            <ModalBody>
              {errorMessage &&
              <h6 className="text-danger">{errorMessage}</h6>
              }
              <FormGroup>
                <Label>Username</Label>
                <Field name="username">
                  {({field, meta}: FieldProps) => (
                    <Input {...field}
                           placeholder="Username"
                           invalid={!!(meta.touched && meta.error)}/>
                  )}
                </Field>
                <FormFeedback>{formikProps.errors.username}</FormFeedback>
              </FormGroup>
              <FormGroup>
                <Label>Roles</Label>
                <Field name="isAdmin">
                  {({field}: FieldProps) => (
                    <CustomInput  {...field} type="switch" id="isAdminSwitch" label="Admin" checked={field.value}/>
                  )}
                </Field>
              </FormGroup>
              <FormGroup>
                <Label>User Status</Label>
                <Field name="enabled">
                  {({field}: FieldProps) => (
                    <CustomInput  {...field} type="switch" id="enabledSwitch" label="Enabled" checked={field.value}/>
                  )}
                </Field>
                <Field name="accountLocked">
                  {({field}: FieldProps) => (
                    <CustomInput  {...field} type="switch" id="accountLockedSwitch" label="Locked" checked={field.value}/>
                  )}
                </Field>
                <Field name="credentialsExpired">
                  {({field}: FieldProps) => (
                    <CustomInput  {...field} type="switch" id="credentialsExpiredSwitch" label="Credentials Expired" checked={field.value}/>
                  )}
                </Field>
                <Field name="accountExpired">
                  {({field}: FieldProps) => (
                    <CustomInput  {...field} type="switch" id="accountExpiredSwitch" label="Expired" checked={field.value}/>
                  )}
                </Field>
              </FormGroup>
            </ModalBody>
            <ModalFooter>
              <Button color="primary"
                      onClick={formikProps.handleSubmit}
                      disabled={!formikProps.dirty || !formikProps.isValid || formikProps.isSubmitting}>
                {mode === 'Add' ? 'Add User' : 'Save User'}
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

export default UserModal;