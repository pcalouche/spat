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
import {Field, Formik} from 'formik';
import * as Yup from 'yup';
import {authApi, userApi} from '../api';
import {useAppContext} from '../hooks';

const Login = () => {
  const history = useHistory();
  const {currentUser, setCurrentUser} = useAppContext();
  const [errorMessage, setErrorMessage] = useState(undefined);

  const handleSubmit = async (formikValues, formikActions) => {
    try {
      // Try to login. If successful a token information will be returned that needs to be added to local storage.
      const token = await authApi.login(formikValues);
      localStorage.setItem('token', token);
      // Get the current user's information next
      const user = await userApi.currentUser();
      // Setting the current user will re-render the routes to be the available in App.js
      formikActions.setSubmitting(false);
      setCurrentUser(user);
    } catch (error) {
      formikActions.setSubmitting(false);
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
              <Formik initialValues={{username: '', password: ''}}
                      validationSchema={
                        Yup.object().shape({
                          username: Yup.string().required('Required'),
                          password: Yup.string().required('Required')
                        })
                      }
                      onSubmit={handleSubmit}>
                {(formikProps) =>
                  <Form onSubmit={formikProps.handleSubmit}>
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
                      <Label>Password</Label>
                      <Field name="password">
                        {({field, meta}) => (
                          <Input {...field}
                                 type="password"
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
                        {/*disabled={!formikProps.dirty || !formikProps.isValid || formikProps.isSubmitting}>*/}
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