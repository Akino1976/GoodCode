import React from 'react';
import {render} from 'react-dom';

class Upload extends React.Component {
  constructor(props){
    super(props)

  }
//*https://stackoverflow.com/questions/28750489/upload-file-component-with-reactjs

  uploadFile(e) {
        var fd = new FormData();
        fd.append('file', this.refs.file.files[0]);
        let token = window.Laravel.csrfToken;
        $.ajax({
            url: '/upload',
            data: fd,
            processData: false,
            contentType: false,
            type: 'POST',
             params: {_token:token},
            success: function(data){
                alert(data);
            }
        });
        e.preventDefault()
    };


  render() {
       return (
           <div>
              <form ref="uploadForm" className="uploader" encType="multipart/form-data" >
                  <input ref="file" type="file" name="file" className="upload-file"/>
                  <input type="button" ref="button" value="Upload" onClick={this.uploadFile.bind(this)} />
              </form>
           </div>
       );
   }

}


export default Upload;
