// Webpack uses this to work with directories
const path = require('path');

// This is the main configuration object.
// Here, you write different options and tell Webpack what to do

const config = (mode) => ({

  // Path to your entry point. From this file Webpack will begin its work
  entry: './render.js',

  // Path and filename of your result bundle.
  // Webpack will bundle all JavaScript into this file
  output: {
    path: path.resolve(__dirname, 'out'),
    publicPath: '',
    filename: mode == "development" ? 'webpack-out.js' : 'webpack-out.min.js'
  },
  module: {
   rules : [
    {
        test: /\.js$/,
        exclude: /(node_modules)/,
        use: {
            loader: 'babel-loader',
            options: {
                presets: ["@babel/preset-env"],
                plugins : [["@babel/plugin-transform-runtime", { "regenerator" : true }]]
            }
        }
    }
   ]
  },


  // Default mode for Webpack is production.
  // Depending on mode Webpack will apply different things
  // on the final bundle. For now, we don't need production's JavaScript 
  // minifying and other things, so let's set mode to development
  mode: 'production'
});

module.exports = (env, argv) => config(argv.mode)
