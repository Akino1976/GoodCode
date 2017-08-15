import React, { Component } from 'react';
import {_} from 'underscore';
import Select from './SelectSurvey.jsx';
import Comp from './comp.jsx';
import axios from  'axios' ;
import PropTypes from 'prop-types';



class SelectTag extends Component {
	constructor(props) {
	 super(props);

	 this.state = {
		 	EffectQuestions			: [],
			EffectAnswers				: [],
			Titel								: ''
	 };
  }

	  componentDidMount() {
			 const self = this,
			 				crm = this.props.crm, /*get its from dashboard.jsx*/
			 	 			url = `/api/distinct/${crm}`;
			 this.serverRequest =
			 	axios.get(url)
						.then(res => {
							if(res.status === 200 && _.size(res.data) > 0)
							{
								let Questions = self.filterData(res.data.EffectQuestions, self.questionKeys) ,
										Answers		= _.pluck(res.data.EffectAnswers, 'surveyName') ,
										crmKey		= _.uniq(
																	_.pluck(res.data.EffectAnswers , 'crmKey')
															).toString();
								/*Clean the output data*/


								const	Questions_1  = _.map(Questions, (arr, id) => {
											return { 	QuestionID 		: arr.QuestionID,
																QuestionString 	: arr.QuestionString.replace(/_/g," ")
											}
									}),
									Answers1		= Answers.map((tag) => { return tag.split(/\_{2}/).shift()} );

									this.setState({ EffectQuestions : Questions_1,
						 											EffectAnswers		: Answers1,
																	Titel  					: crmKey
										}) ;
						}
					});
};


	componentWillUnmount(){
     this.serverRequest.abort();
  };



	filterData( ...args)
	{
		const 	data 		= args[0],
						regExp	= /(Date|Survey|crmKey)/i ;

		return data.filter( (id) =>{
			 return ! regExp.test(id.QuestionString)
		});
	};






	render() {

    return (
      <div>
				<Select
						surveys={this.state.EffectAnswers}
						question={this.state.EffectQuestions}
				/>
					<button
						type="submit"
						className="btn btn-default">
						Submit
					</button>
					<Comp title={this.state.Titel}
					/>
      </div>
    );
  }

} // end of components

SelectTag.propTypes = {
  crm: PropTypes.string.isRequired
};

export default SelectTag;
