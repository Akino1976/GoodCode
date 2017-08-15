import React, { Component } from 'react';
import Select from './SelectSurvey.jsx';

import {_} from 'underscore';


const   url1        =  "/api/survey/create",
        url2        = '/api/distinct',
        crmkey      = 'svenska_john_deere_ab';

let     dataset     = {};

class Mainwrapper extends Component {
  constructor(props){
    super(props)
    this.state = {
      value : "Q_5",
      quedata : []
    }
  }// end of constructor





          componentDidMount() {
              this.extractData(`${url2}/${crmkey}`)
              .then( (res) => {
                if(res.status === 200)
                {

                    dataset = this.fetchData('url2', res.data);

                    this.setState({ quedata : dataset})
                }
              } )
              .catch(function (error) {
                console.log(error);
              });

        }

            extractData(...args){
              let url = args[0] !== undefined ? args[0] : `${url2}/${crmkey}`;
              return axios.get(url);
            };

            filterData( ...args)
          	{
          		const 	data 		= args[0],
          						regExp	= /(Date|Survey|crmKey)/i ;

          		return data.filter( (id) =>{
          			 return ! regExp.test(id.QuestionString)
          		});
          	};


            /*@ arg1 {string} url
              * arg2 {array} dataset associated with url
            */
            fetchData(...args){
              let arg1  = args[0] !== undefined ? args[0] : null,
                  arg2  = args[1] !== undefined ? args[1] : null,
                  tmp   = {};

              if( arg1 === null )
              {
                  return arg1;
              }
              switch (arg1) {
                  case (arg1.match(/^url2$/) || {}).input:
                  /*check to see that all keys are in place*/
                    let testing =   [/EffectQuestions/, /EffectAnswers/]
                              .every(rx => rx.test(_.keys(arg2)));
                      if( testing !== true) return null;
                      tmp = {
                        Questions : this.filterData(arg2.EffectQuestions) ,
                        Answers		: _.pluck(arg2.EffectAnswers, 'surveyName') ,
                        crmKey		: _.uniq(
                                    _.pluck(arg2.EffectAnswers , 'crmKey')
                                ).toString()
                      };
                    break;
                  case (arg1.match(/test/) || {}).input:
                    console.log("Matched the 'test' substring");
                    break;
                  default:
                    console.log("Didn't match");
                    break;
                }
                return tmp;
            }
////////////////////////////////////////////////////////////////////////////////
// change
////////////////////////////////////////////////////////////////////////////////
            change(e)
            {
              e.preventDefault();
              this.setState({
                value : e.target.value.toString()
              })
            }

            render() {

              return (
                <div>

          		    Hellos
                </div>
              );
            }
} // end of Main

export default Mainwrapper;
