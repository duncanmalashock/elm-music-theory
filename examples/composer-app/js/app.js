import { Elm } from '../src/Main.elm';
import { browserPlaybackInit } from "./ports/browserPlayback";
import { midiDevicePlaybackInit } from "./ports/midiDevicePlayback";

// Initialize Elm app
var app = Elm.Main.init();

// Initialize JS ports
browserPlaybackInit(app);
midiDevicePlaybackInit(app);
