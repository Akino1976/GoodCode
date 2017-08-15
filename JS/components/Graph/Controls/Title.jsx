import React, { Component } from 'react';
import PropTypes from 'prop-types';


class Title extends Component{
  constructor(props){
    super(props)
  }



  capitalize(s)
  {
      return s && s[0].toUpperCase() + s.slice(1).replace(/ab$/,"");
  }


  shouldComponentUpdate(nextProps) {
      const differentTitle = this.props.title !== nextProps.title;
      return differentTitle ;
  }

  render() {
    if( this.props.title !== undefined )
    {
       let Name = this.props.title.toString().replace(/_/g,' ');
       Title    = `Welcome ${this.capitalize( Name) } `
    }

    return (
     <div style={{textAlign: 'center'}}>
        <h1>{Title}</h1>
      </div>
    );
  }


} // end of class

Title.propTypes = {
    title: PropTypes.string.isRequired
}



export default Title;
