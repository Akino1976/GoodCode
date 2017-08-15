import React, { Component } from 'react';
import PropTypes from 'prop-types';



class Select extends Component {
  constructor(props) {
	   super(props);
}


filterData( ...args)
{
  const 	data 		= args[0],
          regExp	= /(Date|Survey|crmKey)/i ;

  return data.filter( (id) =>{
     return ! regExp.test(id.QuestionString)
  });
};

  onFieldChange(event) {
      event.preventDefault();
       // for a regular input field, read field name and value from the event
       const fieldName = 'value';
       const fieldValue = event.target.value.toString();
       this.props.onChange(fieldName, fieldValue);
   }


   handleSurveyData(data){
     if(data !== undefined)
     {
        return data.map((tag) => { return tag.split(/\_{2}/).shift()} );
     }
   }


  handleQuestionData(data)
  {
    let self = this;
    if(data !== undefined)
    {
        let Questions = self.filterData(data.Questions);
        const	Questions_1  = _.map(Questions, (arr, id) => {
              return { 	QuestionID 		   :  arr.QuestionID,
                        QuestionString 	 :  arr.QuestionString.replace(/_/g," ")
              }
          });

          return Questions_1;
    }

  }



  render() {
    let  questionDT      = this.handleQuestionData( this.props.data);

          return (
            <div className="form-group">
              <select
                  id        ="question"
                  className ="form-select"
                  value     ={ this.props.value ? this.props.value : 'Q_5'}
                  onChange  = { this.onFieldChange.bind(this) }>
                  <option value="" disabled>
                    {"Frågorställningar"}
                  </option>
                  {questionDT.map( (opt) =>{
                      return (
                        <option
                          key={opt.QuestionID.toString()}
                          value={opt.QuestionID}>
                        {opt.QuestionString}</option>
                      );
                  })}

                </select>
              </div>


          )


        }
}

Select.propTypes = {
    value: PropTypes.string
}


export default Select;
