import { Elm } from '../src/Main.elm';
import abcjs from 'abcjs';

var app = Elm.Main.init();

app.ports.abcOutput.subscribe(function (input) {
  window.requestAnimationFrame(function () {
    const abcData = input
    abcjs.renderAbc("abcViewer", abcData)
  })
});
