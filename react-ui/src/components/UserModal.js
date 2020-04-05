import React, {useState} from 'react';
import PropTypes from 'prop-types';
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
import {Field, Formik} from 'formik';
import * as Yup from 'yup';

import {userApi} from '../api';

const UserModal = ({isOpen, mode, user, submitCallback, cancelCallback}) => {
  const [errorMessage, setErrorMessage] = useState(undefined);

  const initialValues = {
    username: user.username,
    isAdmin: user.roles.filter(role => role.name === 'Admin').length === 1,
    enabled: user.enabled,
    accountLocked: !user.accountNonLocked,
    credentialsExpired: !user.credentialsNonExpired,
    accountExpired: !user.accountNonExpired
  };

  const validationSchema = Yup.object().shape({
    username: Yup.string().required('Required'),
    isAdmin: Yup.bool(),
    enabled: Yup.bool(),
    accountLocked: Yup.bool(),
    credentialsExpired: Yup.bool(),
    accountExpired: Yup.bool()
  });

  const handleSubmit = async (formikValues, formikActions) => {
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
          <ModalHeader>{mode === 'Add' ? 'Add User' : 'Edit User'}</ModalHeader>
          <Form onSubmit={formikProps.handleSubmit}>
            <ModalBody>
              {errorMessage &&
              <h6 className="text-danger">{errorMessage}</h6>
              }
              <FormGroup>
                <Label>Username</Label>
                <Field name="username">
                  {({field, meta}) => (
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
                  {({field}) => (
                    <CustomInput  {...field} type="switch" id="isAdminSwitch" label="Admin" checked={field.value}/>
                  )}
                </Field>
              </FormGroup>
              <FormGroup>
                <Label>User Status</Label>
                <Field name="enabled">
                  {({field}) => (
                    <CustomInput  {...field} type="switch" id="enabledSwitch" label="Enabled" checked={field.value}/>
                  )}
                </Field>
                <Field name="accountLocked">
                  {({field}) => (
                    <CustomInput  {...field} type="switch" id="accountLockedSwitch" label="Locked" checked={field.value}/>
                  )}
                </Field>
                <Field name="credentialsExpired">
                  {({field}) => (
                    <CustomInput  {...field} type="switch" id="credentialsExpiredSwitch" label="Credentials Expired" checked={field.value}/>
                  )}
                </Field>
                <Field name="accountExpired">
                  {({field}) => (
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

UserModal.propTypes = {
  isOpen: PropTypes.bool.isRequired,
  mode: PropTypes.oneOf(['Add', 'Edit']).isRequired,
  user: PropTypes.object.isRequired,
  submitCallback: PropTypes.func.isRequired,
  cancelCallback: PropTypes.func.isRequired
};

UserModal.defaultProps = {
  mode: 'Add'
};

export default UserModal;