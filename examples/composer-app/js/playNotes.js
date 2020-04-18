export { playNotesInit }

import WebAudioFontPlayer from "webaudiofont"

const playNotesInit = function(app) {
  const AudioContextFunc = window.AudioContext || window.webkitAudioContext;
  const audioContext = new AudioContextFunc();
  const player = new WebAudioFontPlayer();
  app.ports.play.subscribe(function (events) {
    const audioFontId = 47;
    const volume = 50 / 127;
    const instrumentInfo = player.loader.instrumentInfo(audioFontId);
    player.loader.startLoad(audioContext, instrumentInfo.url, instrumentInfo.variable);
    player.loader.waitLoad(function () {
      player.cancelQueue(audioContext);
      const startTime = audioContext.currentTime;
      events.forEach(function (event) {
        const time = startTime + event.time;
        player.queueWaveTable(
          audioContext,
          audioContext.destination,
          window[instrumentInfo.variable],
          time,
          event.pitch,
          event.duration,
          volume
        )
      })
    });
    return false
  });
};
