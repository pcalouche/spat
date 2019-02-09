import React                        from 'react';
import {FormFeedback, Input, Label} from 'reactstrap';

const FormikInputWrapper = (props) => {
  const {
    field,
    form,
    labelText,
    ...miscProps
  } = props;

  return (
      <React.Fragment>
        {props.type !== 'checkbox' && props.type !== 'radio' &&
        <Label>{labelText}</Label>}
        <Input {...field} {...miscProps} invalid={form.touched[field.name] && form.errors[field.name]}/>
        <FormFeedback>{form.errors[field.name]}</FormFeedback>
      </React.Fragment>
  );

};

export default FormikInputWrapper;