{-# LANGUAGE QuasiQuotes #-}

module Network.Socketed.Application.Plocketed.Template (plotHtml) where

import Data.List (intercalate)

import Network.Socketed.Internal (showWSHost, stringQuote)
import Network.Socketed.Template (socketedScript)
import Network.Socketed.Application.Plocketed.Internal (PlocketedOptions(..))

plotHtml :: PlocketedOptions -> String
plotHtml opts@(PlocketedOptions wp wh) = intercalate "" $ lines [stringQuote|

var script = document.createElement('script');
script.src = '//www.chartjs.org/assets/Chart.min.js';

script.onload = function() {
   var canvas = document.createElement('canvas');
   canvas.setAttribute('height', '300');
   canvas.style.width = '80%';
   document.body.appendChild(canvas);

   var
       ctx = canvas.getContext('2d'),
       startingData = {
         labels: [0],
         datasets: [
             {
                 fillColor: "rgba(151,187,205,0.2)",
                 strokeColor: "rgba(151,187,205,1)",
                 pointColor: "rgba(151,187,205,1)",
                 pointStrokeColor: "#fff",
                 data: [0]
             }
         ]
       },
       latestLabel = startingData.labels[startingData.labels - 1];

   var chart = new Chart(ctx).Line(startingData, { animationSteps: 15 });

   window.handleMessage = function(e) {
      var vals = e.data.split(' ').map(x => x.trim()).map(x => parseInt(x, 10));

      chart.addData([vals[1]], ++latestLabel);
      latestLabel > 10 && chart.removeData();
   };

   window.handleMessageError = function(e) {
      console.log(e);
   };
};

document.body.appendChild(script);

|]
