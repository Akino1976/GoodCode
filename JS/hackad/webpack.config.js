const path = require('path'),
			webpack = require('webpack');
var dir_build 		= path.resolve(__dirname, 'public/js');

module.exports = {
	entry : {
		"uploadEntry" : './resources/assets/js/upload.js',
		"leedEntry"		: './resources/assets/js/leeds.js',
	},
	output: {
		path			: dir_build,
		filename 	: "[name].js"
	},
	module: {
			loaders: [
					{
							loader: 'babel-loader',
							test: /\.js$/,
							exclude: /(node_modules)/,
							query: {
				 				presets: ['es2015', 'stage-0']
			 			}
					}
			]
	},
	stats: {
         colors: true
  },
	 	devtool: 'source-map',
  	devServer: {
        contentBase: dir_build,
    },

} /*end of module*/
