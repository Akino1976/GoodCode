import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import {  VictoryPie,
          VictoryLabel,
          VictoryTheme,
          VictoryLegend,
          VictoryTooltip } from 'victory';
import stats from 'stats-lite';



class Pie extends Component {
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
    let data  = this.props.data,
        _w    = 800,
        _h    = 400;

    const  _color   = ["#6495ED", "#BDB76B", "#483D8B", "#DAA520", "#ADD8E6" ];

    return (
      <div>
        <h1>Victory Pie</h1>
        <svg width={_w} height={_h}>

        <VictoryPie
          width={_w *(2/3) }
          data={data.graphdata}
          colorScale={_color}
          innerRadius={68} labelRadius={100}
          style={{ labels: { fontSize: 20, fill: "white"}}}
          startAngle={0}
          endAngle={360}
          animate={{duration: 2000}}

        />
        <VictoryLegend
          colorScale={_color}
          data={data.legend}
/>
        <VictoryLabel
          textAnchor="middle"
          x={_w*(1/3)} y={_h/2}
          text="Q2"
        />
      </svg>
      </div>
    );
  }


} // end of class

export default Pie;
