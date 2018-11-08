import React, {Component}                                                 from 'react';
import {connect}                                                          from 'react-redux';
import {withRouter}                                                       from 'react-router-dom';
import {Button, Card, CardBody, CardTitle, Form, FormGroup, Input, Label} from 'reactstrap';

import './Login.css';
import * as actionCreators                                                from '../../redux/actions/auth';
import {Field, Formik}                                                    from 'formik';
import * as Yup                                                           from 'yup';

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
          <Label className="error-text">Error: {this.props.errorMessage}</Label>
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
              username: Yup.string().required(),
              password: Yup.string().required()
            })}
            onSubmit={this.handleSubmit}
            render={formikProps => (
              <Form onSubmit={formikProps.handleSubmit}>
                {errorInfo}
                <FormGroup>
                  <Label>Username</Label>
                  {/*<Input name="username" placeholder="Username" onChange={formikProps.handleChange} onBlur={formikProps.handleBlur}/>*/}
                  <Field
                    name="username"
                    render={(props) => <Input {...props.field} placeholder="Username"/>}
                  />
                </FormGroup>
                <FormGroup>
                  <Label>Password</Label>
                  {/*<Input type="password" name="password" placeholder="Password" onChange={formikProps.handleChange} onBlur={formikProps.handleBlur}/>*/}
                  <Field
                    name="password"
                    render={(props) => <Input type="password" {...props.field} placeholder="Password"/>}
                  />
                  {/*<p>Test {formikProps.errors.password}</p>*/}
                  {/*<Field type="password" name="password" placeholder="Password"/>*/}
                </FormGroup>
                <FormGroup>
                  <Button type="submit" color="primary" block disabled={!formikProps.isValid || formikProps.isSubmitting}>Login</Button>
                </FormGroup>
                <div>
                  {JSON.stringify(formikProps, null, 2)}
                </div>
              </Form>
            )}
          />
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