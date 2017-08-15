import React, { Component } from 'react';


const imgStyle = {
   display: 'inline-block',
   width:'75%'
 },
 divStyle = {
   borderStyle: 'solid',
   borderWidth: 'thin',
   borderColor: '#FFFFFF'
 }

class Img extends Component {
  constructor(props) {
    super(props);

 }
/*TODO: make this to using survey so that it changes add when pressed  */

componentWillReceiveProps(newProps) {

}
 handlePropsData(...args){
   let data = args[0] !== undefined ? args[0]:null;
   return { "type" : data.type,
            "img"  : data.img
          }
 }

///////////////////////////////////////////////////////////////////////////

  render() {
    let dataset = this.handlePropsData(this.props.data);
    return (
        <div style={divStyle}>
          <img style={imgStyle}
              src={`data:image/${dataset.type};base64,${dataset.img}`} >
          </img>
        </div>
    );
  }
  ///////////////////////////////////////////////////////////////////////////


} // end of class 



export default Img;
