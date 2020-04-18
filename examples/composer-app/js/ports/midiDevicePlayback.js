export { midiDevicePlaybackInit }

import WebMidi from "webmidi";

const midiDevicePlaybackInit = function(app) {
  WebMidi.enable(function (err) {
    if (err) {
      console.log("WebMidi could not be enabled.", err);
    } else {
      console.log(WebMidi.inputs);
      console.log(WebMidi.outputs);
    }
  });
};
