import React, {useEffect, useState} from 'react';
import {useHistory} from 'react-router-dom';
import {Button, Card, Col, Container, Form, Row} from 'react-bootstrap';
import {Field, FieldProps, Formik, FormikHelpers, FormikProps} from 'formik';
import * as Yup from 'yup';

import {authApi, userApi} from '../api';
import {useAppContext} from '../hooks';

type LoginFormFields = {
  username: string
  password: string
}

const Login: React.FC = () => {
  const history = useHistory();
  const {currentUser, setCurrentUser} = useAppContext();
  const [errorMessage, setErrorMessage] = useState<string>('');

  const form: {initialValues: LoginFormFields, validationSchema: Yup.AnyObjectSchema} = {
    initialValues: {
      username: '',
      password: ''
    },
    validationSchema: Yup.object().shape({
      username: Yup.string().required('Required'),
      password: Yup.string().required('Required')
    })
  };

  const handleSubmit = async (values: LoginFormFields, formikHelpers: FormikHelpers<LoginFormFields>) => {
    try {
      // Try to login. If successful a token information will be returned that needs to be added to local storage.
      const token = await authApi.login(values.username, values.password);
      localStorage.setItem('token', token);
      // Get the current user's information next
      const user = await userApi.currentUser();
      // Setting the current user will re-render the routes to be the available in App.tsx
      formikHelpers.setSubmitting(false);
      setCurrentUser(user);
    } catch (error) {
      formikHelpers.setSubmitting(false);
      if (error instanceof TypeError) {
        setErrorMessage('Unable to connect to service.');
      } else {
        setErrorMessage(error.message);
      }
    }
  };

  useEffect(() => {
    if (currentUser) {
      history.push('/users');
    }
  }, [currentUser, history]);

  return (
    <Container fluid className="Login mt-5">
      <Row>
        <Col>
          <Card className="m-auto">
            <Card.Header className="font-weight-bold text-center">Login</Card.Header>
            <Card.Body>
              {errorMessage && <p className="text-center text-danger">{errorMessage}</p>}
              <Formik initialValues={form.initialValues}
                      validationSchema={form.validationSchema}
                      onSubmit={handleSubmit}>
                {(formikProps: FormikProps<LoginFormFields>) =>
                  <Form onSubmit={formikProps.handleSubmit}>
                    <Form.Group controlId="username">
                      <Form.Label>Username</Form.Label>
                      <Field name="username">
                        {({field, meta}: FieldProps) => (
                          <Form.Control {...field}
                                        placeholder="Username"
                                        autoComplete="username"
                                        isInvalid={!!(meta.touched && meta.error)}/>
                        )}
                      </Field>
                      <Form.Control.Feedback type="invalid">
                        {formikProps.errors.username}
                      </Form.Control.Feedback>
                    </Form.Group>
                    <Form.Group controlId="password">
                      <Form.Label>Password</Form.Label>
                      <Field name="password">
                        {({field, meta}: FieldProps) => (
                          <Form.Control {...field}
                                        type="password"
                                        autoComplete="current-password"
                                        placeholder="Password"
                                        isInvalid={!!(meta.touched && meta.error)}/>
                        )}
                      </Field>
                      <Form.Control.Feedback type="invalid">
                        {formikProps.errors.password}
                      </Form.Control.Feedback>
                    </Form.Group>
                    <Form.Group>
                      <Button type="submit"
                              variant="primary"
                              block
                              disabled={!formikProps.isValid || formikProps.isSubmitting}>
                        Login
                      </Button>
                    </Form.Group>
                  </Form>
                }
              </Formik>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    </Container>
  );
};

export default Login;