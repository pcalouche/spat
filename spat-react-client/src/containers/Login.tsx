import React, {useEffect, useState} from 'react';
import {useHistory} from 'react-router-dom';
import {
  Button,
  Card,
  CardBody,
  CardHeader,
  Col,
  Container,
  Form,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Row
} from 'reactstrap';
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

  const form: {initialValues: LoginFormFields, validationSchema: Yup.ObjectSchema} = {
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
            <CardHeader className="font-weight-bold text-center">Login</CardHeader>
            <CardBody>
              {errorMessage && <p className="text-center text-danger">{errorMessage}</p>}
              <Formik initialValues={form.initialValues}
                      validationSchema={form.validationSchema}
                      onSubmit={handleSubmit}>
                {(formikProps: FormikProps<LoginFormFields>) =>
                  <Form onSubmit={formikProps.handleSubmit}>
                    <FormGroup>
                      <Label for="username">Username</Label>
                      <Field name="username">
                        {({field, meta}: FieldProps) => (
                          <Input {...field}
                                 placeholder="Username"
                                 id="username"
                                 autoComplete="username"
                                 invalid={!!(meta.touched && meta.error)}/>
                        )}
                      </Field>
                      <FormFeedback>{formikProps.errors.username}</FormFeedback>
                    </FormGroup>
                    <FormGroup>
                      <Label for="password">Password</Label>
                      <Field name="password">
                        {({field, meta}: FieldProps) => (
                          <Input {...field}
                                 type="password"
                                 id="password"
                                 autoComplete="current-password"
                                 placeholder="Password"
                                 invalid={!!(meta.touched && meta.error)}/>
                        )}
                      </Field>
                      <FormFeedback>{formikProps.errors.username}</FormFeedback>
                    </FormGroup>
                    <FormGroup>
                      <Button type="submit"
                              color="primary"
                              block
                              disabled={!formikProps.isValid || formikProps.isSubmitting}>
                        Login
                      </Button>
                    </FormGroup>
                  </Form>
                }
              </Formik>
            </CardBody>
          </Card>
        </Col>
      </Row>
    </Container>
  );
};

export default Login;