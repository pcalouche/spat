import React, {Component}                                                 from 'react';
import {connect}                                                          from 'react-redux';
import {withRouter}                                                       from 'react-router-dom';
import {Button, Card, CardBody, CardTitle, Form, FormGroup, Input, Label} from 'reactstrap';

import './Login.css';
import * as actionCreators                                                from '../../redux/actions/auth';

class Login extends Component {
  state = {
    username: 'activeAdmin',
    password: 'password'
  };

  handleChange = (event, field) => {
    this.setState({[field]: event.target.value});
  };

  handleSubmit = async event => {
    event.preventDefault();
    await this.props.loginUser(this.state.username, this.state.password, this.props.history);
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
          <Form onSubmit={(e) => this.handleSubmit(e)}>
            {errorInfo}
            <FormGroup>
              <Label>Username</Label>
              <Input name="username" value={this.state.username} placeholder="Username" onChange={(e) => this.handleChange(e, 'username')}/>
            </FormGroup>
            <FormGroup>
              <Label>Password</Label>
              <Input type="password" name="password" value={this.state.password} placeholder="Password" onChange={(e) => this.handleChange(e, 'password')}/>
            </FormGroup>
            <FormGroup>
              <Button type="submit" color="primary" block disabled={!(this.state.username && this.state.password)}>Login</Button>
            </FormGroup>
          </Form>
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