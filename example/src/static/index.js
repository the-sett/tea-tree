// Styles
const TextToSVG = require('text-to-svg');
const SimpleCache = require('./simple-cache').Storage;

const {
  Elm
} = require('../elm/Main.elm');

const app = Elm.Main.init({
  node: document.getElementById('diagram')
});

//-- Ports for converting text to SVG.

const fontCache = new SimpleCache();

app.ports && app.ports.textToSVG &&
  app.ports.textToSVG.subscribe(request => {
    fontCache.async(request.font, {
      set: function(setValue) {
        TextToSVG.load(require('./fonts/' + request.font + '.ttf'), function(err, textToSVG) {
          setValue(textToSVG);
        });
      },
      get: function(value) {
        convertTextToSVG(request, value);
      }
    });
  });


function convertTextToSVG(request, textToSVG) {
  const metrics = textToSVG.getMetrics(request.text, {
    fontSize: request.fontSize,
    kerning: request.kerning,
    letterSpacing: request.letterSpacing
  })

  const pathData = textToSVG.getD(request.text, {
    fontSize: request.fontSize,
    kerning: request.kerning,
    letterSpacing: request.letterSpacing
  })

  const result = {
    id: request.id,
    x: metrics.x,
    y: metrics.y,
    baseline: metrics.baseline,
    width: metrics.width,
    height: metrics.height,
    ascender: metrics.ascender,
    descender: metrics.descender,
    pathData: pathData,
    request: request
  }

  app.ports.textToSVGResponse.send(result);
};
