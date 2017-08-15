'use strict';
import "babel-polyfill";
require('./bootstrap') ;
import Leeds from './class/leedSource.js';
let Leed    = new Leeds("table_1", "table_2" );


let btn     = $('input#sendleads');

$("body").on("click", "table.maintable td[id*='comp'], input#sendleads",  (e) => {
  e.preventDefault();
  if( /^(td)$/i.test(e.target.tagName))
  {
    Leed.AddValue(e.target.id);
  } else if (/^(input)$/i.test(e.target.tagName) ){
   Leed.Sending("table_2", "emailAddress") ;
  }


});

 //$(document).on('mouseover', '#other', function() { alert("hello"); });




console.log(Leed);
