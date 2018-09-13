import React, {Component}                                                 from 'react';
import {Button, Card, CardBody, CardTitle, Form, FormGroup, Input, Label} from 'reactstrap';

import './Login.css';

class Login extends Component {
  state = {
    username: 'activeUser',
    password: 'password',
    showError: false,
    errorMessage: null
  };

  handleChange = (event, field) => {
    this.setState({[field]: event.target.value});
  };

  handleSubmit = (event) => {
    event.preventDefault();
  };

  render() {
    let errorInfo = null;
    if (this.state.showError) {
      errorInfo = (
        <FormGroup>
          <Label className="error-text">Error: {this.state.errorMessage}</Label>
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

export default Login;