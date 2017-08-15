'use strict';
import _ from 'lodash';
const     _id   = Symbol(),
          _id1   = Symbol();

class Leeds {
  constructor(id, id1) {

    this._json  = jsonobj; /*object outside of class*/
    this[_id]   = $(`div#${id}`);
    this[_id1]   = $(`table#${id1}`);
    Object.assign(this, this._json);
    this.main("api/v1/leeds");
  //  this.sayHello   = this.sayHello.bind(this);
  } // end of constructor

/*
  sayHello(){
    let btn = $('button#sendleads');
    btn.click( (e) => { alert( ' say: "hello!"') });
     // 'undefined say 'hello!"';
   }
*/
  main(...args){
    let arg 	= args[0] !== undefined ? args[0] : null,
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
  };
  /*arg {object literal} : Data with keys.output
  */
  createTable( arg )
  {
      let data              =  arg !== undefined ? arg.output : null,
      tableContent          = this._json , // end of json
            colNames        = Object.keys(tableContent['colNames']),
            colNamesRev     = Object.keys(tableContent['colNames']).reverse(),
            parentDiv       = this[_id] ;

      const rowsNames       =  Object.keys( tableContent.leads.datafield ) ,
            leadsCompany    = _.map(data, 'leed_company');


            if( data === null)
            {
                    throw "Error elements is missing";
            }

        let aTable = $("<table border='1' style='text-align:center;'>")
                .addClass("table table-striped table-hover maintable")
                .prepend( tableContent.captionMessage )
                .appendTo(parentDiv);

        let aHead = $("<thead>").appendTo(aTable);
                  colNamesRev.forEach( (x,i) => {
                    let _name = tableContent['colNames'][x];
                      $("<th>")
                      .attr({
                        "data-container": "body",
    									  "data-toggle":"tooltip",
    									  "data-original-title" : tableContent.leads.datafield[x].description })
                      .prependTo(aHead).html( _name );
        });

        let aBody   = $("<tbody>").appendTo(aTable);
        _.map(data, (array, id) => {
              let row 			= $("<tr/>"),
                  _v        = '';

              colNames.forEach( (x,i) => {
                switch (true) {
                  case /total/i.test(x):
                       _v = Number(array.delivered) + Number(array.ordered);
                      row.append($("<td/>")
                        .html( _v ))	;
                    break;
                    case /^ordered$/i.test(x):
                        _v =  Number(array.ordered);
                        row.append($("<td/>")
                          .addClass('link-td')
                          .attr({
                              "id": `comp_${array['leed_company']}`
                          })
                          .html( _v ))	;
                      break;
                  default:
                      row.append($("<td/>")
                        .html( array[x]  ))	;
                }
              });
              row.appendTo(aBody);
        });


  } // end of createTable



  /*
    Add valuse to table
  */
  AddValue(value){
    let lead      = $("input#leadC"),
        nrlead    = $("input#nrLead"),
        values    = $(`#${value}`).html();
      console.log("Addvalue");
        console.log(values);
    lead.val(
        value.replace(/comp_/, "").toString()
    );
    nrlead.val(values);

    /*
    if( this[_id1].hasClass("starthidden") )
    {

      $("table#table_2, input#emailAddress")
          .toggleClass('starthidden startshow');
    }
    let row 			= $("<tr/>"),
        InputBox = $('<input />').attr({
            type: "text", value: values
        });
    row.append($("<td/>")
      .html(value.replace(/comp_/, "")));

    row.append($("<td/>").append(InputBox));

    row.appendTo(this[_id1]);


    let tBody =  this[_id1].find("tbody"),
        cols  = {"nr" : "Nr of deliverise", "lead":"lead company"};
    */


  } // end of

  /*
    Scrapes html table based on id
  */
  Sending(id, email) {
    let table = $(`table#${id}`).find("tbody").children(),
        nrRow = table.length,
        output = [];
        console.log("Sending");
          console.log(nrRow);
    _.each( table, (array, id ) => {

      let Step1 = $(array).children(),
          _x1    = "",
          _site   = "";
      _.map(Step1, (x,y) => {
        let _x = x.innerHTML;
        if(  /input/i.test(_x) )
        {
            _x1 = Number( $.parseHTML(_x)[0].value ) ;
        } else {
            _site = _x  ;
        }

      });
      let isTrue = _.map(output, 'site')
                .some( (r) => _site.toLowerCase() === r.toLowerCase());
      if( _x1  > 0 && ! isTrue ){
        output.push({ "site" : _site,
                      "value": _x1 });
      }
    });
    var w              = window.open();
    w.location.href    = `api/v1/leeds/inserting/${JSON.stringify(output)}`
    /*
    axios.post("api/v1/leeds/inserting",
        {"json": JSON.stringify(output)},
      {responseType: 'arraybuffer'}).then(res => {
        console.log(res.data);

    })

    .catch(error => console.log(error));
*/
  } // end of Sending



} // end of class

const jsonobj = {
  "leads" : {
    "datafield": {
      "delivered" : {
          "type"    : "Numeric" ,
          "description" : "Total number of leads in total that has been delivered to TM from company"
        },
        "lastDelivered" : {
            "type"    : "Date" ,
            "description" : "Last date when protectme delivered leads tom TM"
        },
        "leed_company" : {
            "type"    : "string" ,
            "description" : "Company from where leads were recived"
        },
        "ordered" : {
            "type"    : "Numeric" ,
            "description" : "Number of leads left to upload for TM"
        },
        "total" : {
            "type"    : "Numeric" ,
            "description" : "Total leads"
        },
    },
  },
    "colNames": { "leed_company"  : "Lead company",
                  "lastDelivered" : "Last delivered date",
                  "delivered"     : "Nr of delivered",
                  "ordered"       :"Nr of orderd",
                  "total"         :"Total leads"},
    "captionMessage" : "<caption><b>Structure of leads</b></caption>"
};


export default Leeds;
