{-# LANGUAGE QuasiQuotes #-}

module Network.Socketed.Application.Plocketed.Template (plotHtml) where

import Data.List (intercalate)

import Network.Socketed.Internal (stringQuote)
import Network.Socketed.Application.Plocketed.Internal (PlocketedOptions(..))

-- width
-- height
-- data points to keep

plotHtml :: PlocketedOptions -> String
plotHtml (PlocketedOptions w h m) = intercalate "" $ lines $ [stringQuote|

(function(width, height, maxDataAmount) {

var script = document.createElement('script');
script.src = '//www.chartjs.org/assets/Chart.min.js';

script.onload = function() {
   var canvas = document.createElement('canvas');
   canvas.setAttribute('width', width);
   canvas.setAttribute('height', height);
   document.body.appendChild(canvas);

   var legend = document.createElement('div');
   document.body.appendChild(legend);

   var
      ctx = canvas.getContext('2d'),
      seq = 0,
      dataMap = {}, /* data key to data set */
      chart;

   var colors = [
      '#00ffff', '#f0ffff', '#f5f5dc', '#000000', '#0000ff', '#a52a2a',
      '#00ffff', '#00008b', '#008b8b', '#a9a9a9', '#006400', '#bdb76b',
      '#8b008b', '#556b2f', '#ff8c00', '#9932cc', '#8b0000', '#e9967a',
      '#9400d3', '#ff00ff', '#ffd700', '#008000', '#4b0082', '#f0e68c',
      '#add8e6', '#e0ffff', '#90ee90', '#d3d3d3', '#ffb6c1', '#ffffe0',
      '#00ff00', '#ff00ff', '#800000', '#000080', '#808000', '#ffa500',
      '#ffc0cb', '#800080', '#800080', '#ff0000', '#c0c0c0'
   ];

   var randomColorFrag = function() {
      var c = colors[
         Math.floor(Math.random() * colors.length * 10) % colors.length
      ].substring(1);

      return 'rgba('
         + [c.substring(0,2), c.substring(2,4), c.substring(4)]
            .map(v => parseInt(v, 16))
            .join(',');
   };

   var blend = function(frag, a) {
      return frag + ',' + a + ')';
   };

   var mkDataSet = function(label) {
      var c = randomColorFrag();

      return {
        fillColor: blend(c, .2),
        strokeColor: blend(c, .7),
        pointColor: blend(c, .1),
        pointStrokeColor: "#fff",
        points: [],
        label: label
      };
   };

   var mkChart = function() {
      if (chart) {
         chart.clear();
         chart.destroy();
      }

      seq = 0;
      var ds = Object.keys(dataMap)
         .map(k => dataMap[k])
         .sort((a, b) => a.idx - b.idx)
         .map(set => {

            /* trim to the last element */
            set.data = [set.data[set.data.length - 1]];

            return Object.assign(
               mkDataSet(set.key),
               { data: set.data }
            );
         });

      console.log(JSON.stringify(ds));

      chart
         = window.chart
         = new Chart(ctx).Line(
            {
               labels: [seq],
               datasets: ds
            },
            {
               animationSteps: 15,
               legendTemplate: '<table><tr>'
                   +'<% for (var i=0; i<datasets.length; i++) { %>'
                   +'<td><div class=\"boxx\" '
                     + 'style=\"background-color:<%=datasets[i].fillColor %>\">'
                   +'<% if (datasets[i].label) '
                     + '{ %> <%= datasets[i].label %> <% } %></div></td>'
                   +'<% } %>'
                   +'</tr></table>'
            }
         );

      legend.innerHTML = chart.generateLegend();
   };

   window.dataMap = dataMap;

   window.handleMessage = function(e) {
      var
         vals = e.data.split(' ').map(x => x.trim()).map(x => parseInt(x, 10)),
         key = vals[0],
         val = vals[1];

      if (!(key in dataMap)) {
         dataMap[key] = {
            key: key,
            idx: Object.keys(dataMap).length,
            data: [val]
         };

         mkChart();
      } else {
         var newPoints = Object.keys(dataMap)
            .map(k => dataMap[k])
            .sort((a, b) => a.idx - b.idx)
            .map(set => {
               if (set.key === key) {
                  set.data.push(val);
               } else {
                  set.data.push(set.data[set.data.length - 1]);
               }

               return set.data[set.data.length - 1];
            });

         chart.addData(newPoints, ++seq);
      }

      if (seq > maxDataAmount) {
         chart.removeData();

         newPoints = Object.keys(dataMap).map(k => {
            dataMap[k].data.shift();
         });
      }
   };

   window.handleMessageError = function(e, err) {
      console.log(e, err);
   };
};

document.body.appendChild(script);

})(
|] ++ (intercalate "," $ map show [w, h, m]) ++ ");"
