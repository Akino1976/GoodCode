import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import Bar from './Bar.jsx';
import Pie from './Pie.jsx';




class Graphs extends Component {
  constructor(props) {
    super(props);
    this.state = {
      "grahpData": this.handleData(props.data),
      "graphtype" : this.decideChart(props.value)
    };

 }


/////////////////////////////////////////////
// Components
/////////////////////////////////////////////
  componentWillReceiveProps(newProps) {
    let tmpdata = this.handleData(newProps.data),
        tmptype = this.decideChart(newProps.value);

        this.setState({
              "grahpData" : tmpdata,
              "graphtype" : tmptype
          });
  }// end of componentWillReceiveProps

  shouldComponentUpdate(nextProps) {
      const diff = this.props.value !== nextProps.value;
      return diff ;
  }

/////////////////////////////////////////////
// function
/////////////////////////////////////////////

  /*Function for deciding the chart type*/
  decideChart(arg){
      let regexp = new RegExp( arg ,"i"),
          output = [];
      const chartCondition = {
        "bar"     : ["Q_5", "Q_13", "Q_14", "Q_6"],
        "stack"   : ["Q_7", "Q_8", "Q_9"],
        "pie"     : ["Q_10", "Q_11" , "Q_2", "Q_4"],
        "spider"  : ["Q_12"]
      };
      // get only those that are keyed
      _.map(chartCondition, (array, key) => {
          if( array.some(x => regexp.test(x))){
            return output.push(key) ;
          }
      });


      return output.toString();
  }

  /*Calulate each occurence of props*/
  handleData( props){
    let dataset     = props !== undefined ? props: null,
        legend      = [],
        counts      = {},
        graph       = [],
        out         = {};
    if(dataset === null )
    {
      throw "Nothing inside dataset";
    }
    //Count all elements
    _.map(props, (val, key) => {
      // must be defined
      if (typeof counts[val] === "undefined") {
        counts[val] = 1;
      } else {
        counts[val]++;
      }
    });
    // Make into victory chart
   _.map(counts, (y1, x1) => {
          /*if string then make it false*/
          let x2 = /[a-ö]/i.test(x1) ? false :
                    Number(x1.replace(/([0-9]+)?-([0-9]+)/, "$2")),
              x3 = x1 === "" ? "NA" : x1 ;
          graph.push( {
                    "x": x3  ,
                    "y" : Number(y1),
                    "x1" : x2 ,
                   "label" : y1.toString() });
          legend.push({
                "name" : x3
          });
    });
    out       = { "graphdata" :graph,
                  "legend"    : legend}
    return out;
  }

/////////////////////////////////////////////
// Render
render() {
      let renderGraph = ''
      switch (this.state.graphtype) {
        case 'bar':
            renderGraph = <Bar data={ this.state.grahpData } />
        break;
        case 'pie':
            renderGraph = <Pie data={ this.state.grahpData } />
        break;
        case 'spider':
            renderGraph = <Spider data={ this.state.grahpData } />
        break;

      }
   return (
      renderGraph
   )
  }
} // end of class

/////////////////////////////////////////////
export default Graphs;
