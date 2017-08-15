import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import {  VictoryChart,
          VictoryGroup,
          VictoryPolarAxis,
          VictoryTheme,
          VictoryArea ,
          VictoryLabel ,
          VictoryTooltip } from 'victory';
import stats from 'stats-lite';


const axisValues = ['Positiv',
                    'Förtroendegivande',
                    'Kul',
                    'Unik',
                    'Överraskande',
                    'Seriös',
                    'Intresseväckande',
                    'Tråkig',
                    'Osympatisk',
                    'Fånig',
                    'Irriterande',
                    'Ointressant',
                    'Negativ',
                    'Stälstruken',
                    'Misstänksam',
                    'Varm/omtänksam'],
      api       = '/api/Questionsurvey';


const characterData1 = [
                      { strength: 1, intelligence: 250, luck: 1, stealth: 40, charisma: 50 },
                      { strength: 2, intelligence: 300, luck: 2, stealth: 80, charisma: 90 },
                      { strength: 5, intelligence: 225, luck: 3, stealth: 60, charisma: 120 }
                    ];

class Spider extends Component {
  constructor(props) {
    super(props);
    this.state = {
      data:  [],
      maxima: [ ]

    };
  } // end of constructor

//////////////////////////////////////////////////////////////////////////////////////////
// Components
//////////////////////////////////////////////////////////////////////////////////////////

componentWillMount( ) {
      this.getDataset(this.props);
}

  componentWillReceiveProps(...newProps) {
      let tmpdata = this.getDataset(...newProps);

/*
          this.setState({
                "grahpData" : tmpdata,
                "graphtype" : tmptype
            });
            */
    }// end of componentWillReceiveProps

    shouldComponentUpdate(...newProps) {
        const diff = this.props[0].value !== nextProps[0].value;
        return diff ;
    }
//////////////////////////////////////////////////////////////////////////////////////////

  getDataset(...prop){

    let self          = this,
        data          = prop !== undefined ? prop[0] : null,
        characterData = [];
    if( data === null)
    {
      throw "Nothing inside props";
    }

        axios.post(`${api}`, { "crm"     : `${data.crm}`,
                                "survey"  : `${data.survey}`,
                                "question" : `${data.value}`})
        .then( (res) => {
        if(res.status === 200 &&  _.size(_.keys(res.data) ) > 1 )
        {
            let arrayKeys     = _.toArray(_.keys(res.data)) ;
            arrayKeys.forEach( (_k) => {
                let output = {};
                _.map(res.data[_k], (value, id) => {
                  if(! /total/i.test(value.Q_12)) {

                      output[`${value.Q_12}`]  =  value.antal !== undefined ?
                                              Number(value.antal): null ;
                  }
                });
                characterData.push( output  );
            }); // end of foreach

      } // end of if
        this.setState({ "data"    :  characterData,
                        "maxima"  :  self.getMaxima(characterData)
                  });

      })
      .catch(function (error) {
        console.log(error.response);
      });



  } // end of getDataset

  /*
  Function that returns the max if there is multiple datasource
  */
  getMaxima(data) {
      const groupedData = Object.keys( data[0] ).reduce((memo, key) => {
        memo[key] = data.map((d) => d[key]);
        return memo;
      }, {});
      return Object.keys(groupedData).reduce((memo, key) => {
        memo[key] = Math.max(...groupedData[key]);
        return memo;
      }, {});
  }

  processData(data) {
    const maxByGroup = this.getMaxima(data);
    const makeDataArray = (d) => {
      return Object.keys(d).map((key) => {
        return { x: key, y: d[key] / maxByGroup[key] };
      });
    };
    return data.map((datum) => makeDataArray(datum));
  }

  render() {
   return (
     <VictoryChart polar
       theme={VictoryTheme.material}
       domain={{ y: [ 0, 1 ] }}
     >
       <VictoryGroup colorScale={["gold", "orange", "tomato"]}
         style={{ data: { fillOpacity: 0.2, strokeWidth: 2 } }}
       >
         {_.map(this.state.data, (data, i) => {
           return <VictoryArea key={i} data={data}/>;
         })}
       </VictoryGroup>
     {
       Object.keys(this.state.maxima).map((key, i) => {
         return (
           <VictoryPolarAxis key={i} dependentAxis
             style={{
               axisLabel: { padding: 10 },
               axis: { stroke: "none" },
               grid: { stroke: "grey", strokeWidth: 0.25, opacity: 0.5 }
             }}
             tickLabelComponent={
               <VictoryLabel labelPlacement="vertical"/>
             }
             labelPlacement="perpendicular"
             axisValue={i + 1} label={key}
             tickFormat={(t) => Math.ceil(t * this.state.maxima[key])}
             tickValues={[0.25, 0.5, 0.75]}
           />
         );
       })
     }
       <VictoryPolarAxis
         labelPlacement="parallel"
         tickFormat={() => ""}
         style={{
           axis: { stroke: "none" },
           grid: { stroke: "grey", opacity: 0.5 }
         }}
       />

     </VictoryChart>
   );
 }


} // end of class

export default Spider;
