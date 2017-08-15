'use strict';
import _ from 'lodash';
const _url = Symbol(),
      _id  = Symbol();




class ExtractData {
  constructor( id ) {
    this[_url]  = '/api/vi/upload/create';
    this._json  = jsonobj; /*object outside of class*/
    Object.assign(this, this._json);
    this[_id]   = $(`div#${id}`);
    this.main(this[_url]);
  }


  /*when end is done then it will update the table*/



  main(url){
    let arg 	= url !== undefined ? url : `${this[_url]}`,
        self  = this;

      axios.get(`${arg}`)
        .then( (res) => {

          if(res.status === 200){
              console.log(res.data) ;
              self.createTable(res.data);
          }

      })
      .catch(  (error) => {
        console.log(error.response);
      });


  }


  createTable( arg )
  {
    let data =  arg !== undefined ? arg : null,
        tableContent          = this._json , // end of json
                colNames      = tableContent['colNames'],
                colNamesRev   = tableContent['colNames'].reverse(),
                parentDiv     = this[_id] ,
                rowsNames     = _.keys( tableContent.leads.datafield) ,
                needsToTrue   =  rowsNames.every(
                  // check each key in current json object a
                    // against the one in DB, every has to be true
                      (r) => data.includes(r) );

    if( data === null || needsToTrue === false)
    {
      throw "Error elements is missing";
    }

    let tableStep1 =  $("<table border='1' style='text-align:center;'>")
                        .addClass("table table-striped table-hover infotable")
                        .prepend( tableContent['captionMessage'] )
                        .appendTo(   parentDiv  );

    let aHead = $("<thead>").appendTo(tableStep1);
    colNamesRev.forEach( function(x,i){
          $("<th>").prependTo(aHead).html( x );
    });
    let aBody   = $("<tbody>").appendTo(tableStep1);
    rowsNames.forEach( (name, id) => {
        let row   = $("<tr/>"),
            step1 = tableContent.leads.datafield[name] !== undefined ?
                  tableContent.leads.datafield[name] : null;
        if( step1 === null)
        {
          return null;
        }
        let count = 0;
        _.map(step1, (name1, id1) => {
          if( count === 0 )
          {
            row.append(
                $("<td/>")
                  .html(name));
              row.appendTo(aBody);
              ++count;
          }

            row.append(
              $("<td/>")
              .html(name1));

        row.appendTo(aBody);

    });
  })
} // end of createTable()



}


const jsonobj = {
  "leads" : {
    "datafield": {
      "dateuploaded" : {
          "type"    : "date" ,
          "exempel" : "2017-01-01" ,
          "Optional" : "True, upload date is default"
        },
        "firstname" : {
            "type"    : "string" ,
            "exempel" : "Olof" ,
            "Optional" : "False"
        },
        "lastname" : {
            "type"    : "string" ,
            "exempel" : "Andersson" ,
            "Optional" : "False"
        },
        "ssn" : {
            "type"    : "string" ,
            "exempel" : "1999-01-01 1234" ,
            "Optional" : "False, if last 4 digits isn't present then it will not be evaluated"
        },
        "email" : {
            "type"    : "string" ,
            "exempel" : "olof@gmail.com" ,
            "Optional" : "True"
        },
        "zip" : {
            "type"    : "integer" ,
            "exempel" : "12354" ,
            "Optional" : "True"
        },
        "phone" : {
            "type"    : "integer" ,
            "exempel" : "0707771212" ,
            "Optional" : "True, but the first digit (0) can be ommited"
        },
        "surveyprice" : {
            "type"    : "string" ,
            "exempel" : "Willys Presentkort 10.000sek" ,
            "Optional" : "True"
        },
        "source" : {
            "type"    : "string" ,
            "exempel" : "www.somesite.se" ,
            "Optional" : "True"
        }
      },
    },
    "colNames": ['Input field names',
                  'type',
                  'exempel',
                  'Optional, if false then needed' ],
    "captionMessage" : "<caption><b>Structure of input file</b></caption>"
};

export default ExtractData;
