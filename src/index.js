var layout = require('aframe-layout').component;
AFRAME.registerComponent('layout', layout);
var Elm = require( './Main' );
var app = Elm.Main.embed(document.body,config);
