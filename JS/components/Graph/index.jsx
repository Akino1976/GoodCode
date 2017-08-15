import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import Title from './Controls/Title.jsx';
import Select from './Controls/SelectSurvey.jsx';
import Image from './Controls/Image.jsx';
import Spider from './Charts/Spider.jsx';
import Graphs from './Charts/index.jsx';


const   api1        =  "/api/survey/" , // POST
        api2        = '/api/distinct' ,
        survey      = 'ATL_19.2015',
        crm         = 'svenska_john_deere_ab';

// $.post("/api/survey", {"crm": "svenska_john_deere_ab","survey" : "ATL_19.2015"})
class Graph extends Component{
  constructor() {
      super();

      this.state = {
          questionData : [],
          value: 'Q_5'
      };
  }


  onChange(field, value) {
  if(field === null || field === undefined || field === "" )
  {
      field = 'value';
  }
      // parent class change handler is always called with field name and value
      this.setState({[field]: value});
  }

  /* Load surveyData
  */
  componentWillMount() {
      this.loadQuestionData();
  }

  /*
    Loads the survey data based on crm key
    TODO: Make this.crm so that its based on login and also based on surveytype
  */
  loadQuestionData( ...args ) {
    const crmName         = this.props.crm === null ? this.crm : this.props.crm ,
          self            = this;
    let imageData         = {},
        surveyData        = {};

          axios.post(`${api1}`, { "crm"     : `${crmName}`,
                                  "survey"  : `${survey}`})
          .then( (res) => {
          if(res.status === 200)
          {
              let arrayKeys     = _.keys(res.data);
              let cleanQuestion = self.cleanQuestionData(res.data.questionTable);
              if( arrayKeys.some( rx => /answersImg/.test(rx) ) )
              {
                 imageData = {
                                  "type" : res.data.answersImg[0].type,
                                  "img"  : res.data.answersImg[0].img
                              }
              }


              this.setState({ questionData : cleanQuestion,
                              "imageData"  : imageData ? imageData: null,
                              "surveyData" : res.data.answersTable
              })

          }

        })
        .catch(function (error) {
          console.log(error.response);
        });

  } // end of loadQuestionData()

  /*
    used inside loadQuestionData
  */
  cleanQuestionData( data)
  {
    if( _.size(data) == 0)
    {
      throw 'Error';
    }

        return {
          Questions : this.filterData(data) ,
          crmKey		: this.props.crm ?  this.props.crm : this.crm
        };
  }

  /*
    used inside cleanQuestionData
  */
  filterData( ...args)
  {
    const 	data 		= args[0],
            regExp	= /(Date|Survey|crmKey)/i ;

    return data.filter( (id) =>{
       return ! regExp.test(id.QuestionString)
    });
  };


  /*Graph data inserted into Graphs component based on this.state.value
  Groups data by *.value and sends it to component
   */
  extractGraphData(...args)
  {
    let value = args[0] !== undefined ? args[0]: "Q_5";
    return _.map(this.state.surveyData, value);
  }

////////////////////////////////////////////////////////////////////
// Render happens here
////////////////////////////////////////////////////////////////////
  render() {
    const crm1 = this.props.crm === null ? this.crm : this.props.crm ;
    if ( _.size(this.state.questionData) === 0) {
          return (
              <h2>{ `Loading data for survey ${crm1}`} </h2>
          );
    }
    let Out = '';
    if( /^(Q_12)$/i.test(this.state.value) )
    {
      const list = { "value" : this.state.value,
                      "crm"   :  this.props.crm,
                      "survey" : survey };
      Out = <Spider {...list } />
    } else {
      Out = <Graphs data={this.extractGraphData(this.state.value)}
              value={this.state.value}/>
    }

    return(
      <div>
          <Title title={this.state.questionData.crmKey} />
          <Select data={this.state.questionData}
                  value={this.state.value}
                  onChange={this.onChange.bind(this) } />

          { Out }
          <Image data={this.state.imageData} />
      </div>
    )



  }



} // end of class



export default Graph;
