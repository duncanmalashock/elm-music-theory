import { Elm } from '../src/Main.elm';
import { browserPlaybackInit } from "./browserPlayback";

import abcjs from 'abcjs';

var app = Elm.Main.init();

// Initialize JS ports

app.ports.abcOutput.subscribe(function (input) {
  window.requestAnimationFrame(function () {
    const abcData = input
    abcjs.renderAbc("abcViewer", abcData)
  })
});

browserPlaybackInit(app);
