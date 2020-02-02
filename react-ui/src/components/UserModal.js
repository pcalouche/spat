import React from 'react';
import PropTypes from 'prop-types';
import {Button, Col, Form, FormGroup, Input, Label, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';
import {Formik} from 'formik';
import * as Yup from 'yup';

const UserModal = (props) => {
  console.info(props);
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
      initialValues={{
        username: props.user.username ? props.user.username : '',
        accountExpired: props.user.accountNonExpired !== undefined ? !props.user.accountNonExpired : false,
        accountLocked: props.user.accountNonLocked !== undefined ? !props.user.accountNonLocked : false,
        credentialsExpired: props.user.credentialsNonExpired !== undefined ? !props.user.credentialsNonExpired : false,
        enabled: props.user.enabled !== undefined ? props.user.enabled : true,
        isUser: true,
        isAdmin: props.user.roles ? props.user.roles.filter(role => role.name === 'Admin').length === 1 : false
      }}
      enableReinitialize={true}
      validationSchema={Yup.object().shape({
        username: Yup.string().required('Required'),
        accountExpired: Yup.bool(),
        accountLocked: Yup.bool(),
        credentialsExpired: Yup.bool(),
        enabled: Yup.bool(),
        isUser: Yup.bool(),
        isAdmin: Yup.bool()
      })}
      isInitialValid={(formikBag) => {
        return formikBag.validationSchema.isValidSync(formikBag.initialValues);
      }}
      onSubmit={async (values, actions) => {
        let roles = [];
        if (values.isAdmin) {
          roles.push({name: 'Admin'});
        }
        await props.submitCallback({
          username: values.username,
          accountNonExpired: !values.accountExpired,
          accountNonLocked: !values.accountLocked,
          credentialsNonExpired: !values.credentialsExpired,
          enabled: values.enabled,
          roles: roles
        });
        actions.setSubmitting(false);
      }}
    >
      {(formikProps) => (
        <Modal isOpen={props.open} toggle={props.cancelCallback}>
          <ModalHeader toggle={props.cancelCallback}>{modalTitle}</ModalHeader>
          <Form onSubmit={e => e.preventDefault()}>
            <ModalBody>
              {props.errorMessage &&
              <Label className="text-danger font-weight-bold">Error: {props.errorMessage}</Label>}
              {props.mode === 'Delete' &&
              <p>Are you sure you want to delete {props.user.username}?</p>
              }
              {props.mode !== 'Delete' &&
              <React.Fragment>
                <FormGroup row>
                  <Label sm={4}>Username</Label>
                  <Col sm={8}>
                    <Input
                      name="username"
                      placeholder="Username"
                      onChange={formikProps.handleChange}
                      onBlur={formikProps.handleBlur}
                      value={formikProps.values.username}
                      invalid={formikProps.touched.username && formikProps.errors.username}
                      disabled={props.mode === 'Edit'}/>
                  </Col>
                </FormGroup>
                <FormGroup row>
                  <Label sm={4} className="pt-0">Account Status</Label>
                  <Col sm={8}>
                    <FormGroup check>
                      <Label check>
                        <Input
                          type="checkbox"
                          name="accountExpired"
                          onChange={formikProps.handleChange}
                          onBlur={formikProps.handleBlur}
                          checked={formikProps.values.accountExpired}/>Expired
                      </Label>
                    </FormGroup>
                    <FormGroup check>
                      <Label check>
                        <Input
                          type="checkbox"
                          name="accountLocked"
                          onChange={formikProps.handleChange}
                          onBlur={formikProps.handleBlur}
                          checked={formikProps.values.accountLocked}/>Locked
                      </Label>
                    </FormGroup>
                    <FormGroup check>
                      <Label check>
                        <Input
                          type="checkbox"
                          name="credentialsExpired"
                          onChange={formikProps.handleChange}
                          onBlur={formikProps.handleBlur}
                          checked={formikProps.values.credentialsExpired}/>Credentials Expired
                      </Label>
                    </FormGroup>
                    <FormGroup check>
                      <Label check>
                        <Input
                          type="checkbox"
                          name="enabled"
                          onChange={formikProps.handleChange}
                          onBlur={formikProps.handleBlur}
                          checked={formikProps.values.enabled}/>Enabled
                      </Label>
                    </FormGroup>
                  </Col>
                </FormGroup>
                <FormGroup row>
                  <Label sm={4} className="pt-0">Roles</Label>
                  <Col sm={8}>
                    <FormGroup check>
                      <Label check>
                        <Input
                          type="checkbox"
                          name="isAdmin"
                          onChange={formikProps.handleChange}
                          onBlur={formikProps.handleBlur}
                          checked={formikProps.values.isAdmin}/>Admin
                      </Label>
                    </FormGroup>
                  </Col>
                </FormGroup>
              </React.Fragment>
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
    </Formik>
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