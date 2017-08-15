import React, { Component } from 'react';
let Title = '';
class Comp extends Component {
  constructor(props){
    super(props)
  }

capitalize(s)
{
    return s && s[0].toUpperCase() + s.slice(1).replace(/ab$/,"");
}

  render() {
    if( this.props.title !== undefined )
    {
       let Name = this.props.title.toString().replace(/_/g,' ');
       Title    = `Welcome ${this.capitalize(Name) } `
    }

    return (
     <div style={{textAlign: 'center'}}>
        <h1>{Title}</h1>
      </div>
    );
  }
}



export default Comp;
