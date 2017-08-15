'use strict';
import "babel-polyfill";
require('./bootstrap');
import Upload from './class/uploadSource.js';
const btn = $("button#import");



let Extract  = new Upload("root");
/*
btn.on('click', (e) => {
  e.preventDefault();
  console.log(e);
  //$("table.infotable").remove();
  Extract.Hello();
})
/*
if( $("table.infotable").children('tbody').find('td').length > 0 )
{
  Extract.main();
}
*/
console.log(Extract);
