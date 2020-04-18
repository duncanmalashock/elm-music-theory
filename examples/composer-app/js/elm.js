import { Elm } from '../src/Main.elm';
import { playNotesInit } from "./playNotes"

var app = Elm.Main.init({
  node: document.querySelector('main'),
});

playNotesInit(app);
