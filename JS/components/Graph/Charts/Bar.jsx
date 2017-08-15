import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import {  VictoryBar,
          VictoryChart,
          VictoryAxis,
          VictoryTheme,
          VictoryTooltip } from 'victory';
import stats from 'stats-lite';



class Bar extends Component {
  constructor(props) {
    super(props);

 }

 /////////////////////////////////////////////
 // Components
 /////////////////////////////////////////////
   componentWillReceiveProps(newProps) {
     //this.handleData(newProps.data);
   }


/////////////////////////////////////////////
// function
/////////////////////////////////////////////
 //_.groupBy(_.map(res.data.answersTable, 'Q_5'))


  render() {
    let data  = this.props.data.graphdata,
        _w    = 800,
        _h    = 400;
    data.sort((a, b) => {
      if( /[0-9]/i.test(a.x1)){
          return a.x1 - b.x1
      } else {
          return a.y - b.y
      }
    });
    /*if "Q_13" then horiz, threat Q_14 differently*/
    let    tickV = _.map(data, "x");

    return (
      <div>
        <h1>Victory Tutorial</h1>
        <svg width={_w} height={_h}>
        <g>
        <VictoryChart
          width={_w*(2/3)}
          domainPadding={20}
          theme={VictoryTheme.material}
        >
          <VictoryAxis
            tickValues={tickV}
          />
          <VictoryAxis
            dependentAxis
            tickFormat={ ( x ) =>
               (`${x}`)}
          />
          <VictoryBar
          labelComponent={
            <VictoryTooltip
                cornerRadius={(d) => d.x > 6 ? 0 : 20}
                pointerLength={(d) => d.y > 0 ? 5 : 20}
                flyoutStyle={{
                  stroke: (d) => d.x === 10 ?
                    "tomato" : "black"
                }}
              />
            }
            data={data}
            x={"x"}
            y={"y"}
            style={{
                data: {fill: "black", width: 20}
            }}
          />
        </VictoryChart>
        </g>
        </svg>

      </div>
    );
  }


} // end of class

export default Bar;
