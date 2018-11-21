import React, {Component}                                                               from 'react';
import {connect}                                                                        from 'react-redux';
import {withRouter}                                                                     from 'react-router-dom';
import {Button, Card, CardBody, CardTitle, Form, FormFeedback, FormGroup, Input, Label} from 'reactstrap';
import {Formik}                                                                         from 'formik';
import * as Yup                                                                         from 'yup';

import './Login.scss';
import * as actionCreators                                                              from '../../redux/actions/auth';

class Login extends Component {
  handleSubmit = async (values, actions) => {
    await this.props.loginUser(values.username, values.password);
    actions.setSubmitting(false);
    this.props.history.push('/teams');
  };

  render() {
    let errorInfo = null;
    if (this.props.errorMessage) {
      errorInfo = (
          <FormGroup>
            <Label className="text-danger font-weight-bold">Error: {this.props.errorMessage}</Label>
          </FormGroup>
      );
    }

    return (
        <Card className="Login">
          <CardBody>
            <CardTitle>Please Login</CardTitle>
            <Formik
                initialValues={{username: '', password: ''}}
                validationSchema={Yup.object().shape({
                  username: Yup.string().required('Required'),
                  password: Yup.string().required('Required')
                })}
                onSubmit={this.handleSubmit}
            >
              {formikProps => (
                  <Form onSubmit={formikProps.handleSubmit} autoComplete="off">
                    {errorInfo}
                    <FormGroup>
                      <Label>Username</Label>
                      <Input
                          name="username"
                          placeholder="Username"
                          onChange={formikProps.handleChange}
                          onBlur={formikProps.handleBlur}
                          value={formikProps.values.username}
                          invalid={formikProps.touched.username && formikProps.errors.username}/>
                      <FormFeedback>{formikProps.errors.username}</FormFeedback>
                    </FormGroup>
                    <FormGroup>
                      <Input
                          type="password"
                          name="password"
                          placeholder="Password"
                          onChange={formikProps.handleChange}
                          onBlur={formikProps.handleBlur}
                          value={formikProps.values.password}
                          invalid={formikProps.touched.password && formikProps.errors.password}/>
                      <FormFeedback>{formikProps.errors.password}</FormFeedback>
                    </FormGroup>
                    <FormGroup>
                      <Button type="submit" color="primary" block disabled={!formikProps.isValid || formikProps.isSubmitting}>Login</Button>
                    </FormGroup>
                  </Form>
              )}
            </Formik>
          </CardBody>
        </Card>
    );
  }
}

const mapStateToProps = (state) => {
  return {
    errorMessage: state.auth.errorMessage,
    loggedInUser: state.auth.loggedInUser
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    loginUser: (username, password) => dispatch(actionCreators.loginUser(username, password))
  };
};

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(Login));