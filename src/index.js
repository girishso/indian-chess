import './main.css';
import logoPath from './logo.svg';
const { App } = require('./App.elm');

let app = App.embed(document.getElementById('root'), logoPath);

app.ports.alert.subscribe( str => window.alert(str) )
