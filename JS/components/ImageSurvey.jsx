import React, { Component } from 'react';
import {_} from 'underscore';



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
  render() {
    let userImg;
    if( this.props.value !== undefined && _.size(this.props.value) > 0  )
    {
      userImg = (
          <img style={imgStyle}
              src={`data:image/${this.props.type};base64,${this.props.value}`} >
          </img>
      );
    } else {
      userImg = null;
    }
    return (
        <div style={divStyle}>
            {userImg}
        </div>
    );
  }
}
export default Img;
