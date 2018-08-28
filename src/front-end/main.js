/*
 * Penrose web front-end main script
 * @author: Wode "Nimo" Ni
 * @version: 06/23/2017
 */

var DEBUG           = false
// var DEBUG           = true
var CANVAS_WIDTH    = 1000
var CANVAS_HEIGHT   = 1000
var SAMPLE_INTERVAL = 40
var socketAddress   = 'ws://localhost:9160/'

var label_svgs = {}

////////////////////////////////////////////////////////////////////////////////
// Modules
//

//////////////// Utils
var Utils = (function () {

    /**
     * Calculate Tangence from degrees
     */
    function getTanFromDegrees(degrees) {
        return Math.tan(degrees * Math.PI/180);
    }

    /**
     * TODO: add documentation
     */
    function _rgbToHex(r, g, b) {
        function decToHex(c) {
            var hex = c.toString(16);
            return hex.length == 1 ? "0" + hex : hex;
        }
        return "#"
            + decToHex(Math.round(255 * r))
            + decToHex(Math.round(255 * g))
            + decToHex(Math.round(255 * b));
    }

    /**
     * Translate coordinates from polar to cartesian
     * Adopted from: https://codepen.io/AnotherLinuxUser/pen/QEJmkN
     * (Added by Dor)
     */
    function polarToCartesian(centerX, centerY, radius, angleInDegrees) {
        var angleInRadians = ((angleInDegrees - 90) * Math.PI) / 180.0;
        return {
            x: centerX + (radius * Math.cos(angleInRadians)),
            y: centerY + (radius * Math.sin(angleInRadians))
        };
    }

    /*
     * Translate a list of points ([[x1, y1], [x2, y2] ...]) to a
     * Catmull-Rom Spline curve represented as an SVG path string
     * Adopted from: https://codepen.io/osublake/pen/BowJed
     */
     function catmullRomSpline(list, k) {
        // flatten the point list for simplicity
        var data = [].concat.apply([], list);
        if (k == null) k = 1;
        var size = data.length;
        var last = size - 4;
        var path = "M" + [data[0], data[1]];
        for (var i = 0; i < size - 2; i += 2) {
            var x0 = i ? data[i - 2] : data[0];
            var y0 = i ? data[i - 1] : data[1];
            var x1 = data[i + 0];
            var y1 = data[i + 1];
            var x2 = data[i + 2];
            var y2 = data[i + 3];
            var x3 = i !== last ? data[i + 4] : x2;
            var y3 = i !== last ? data[i + 5] : y2;
            var cp1x = x1 + (x2 - x0) / 6 * k;
            var cp1y = y1 + (y2 - y0) / 6 * k;
            var cp2x = x2 - (x3 - x1) / 6 * k;
            var cp2y = y2 - (y3 - y1) / 6 * k;
            path += "C" + [cp1x, cp1y, cp2x, cp2y, x2, y2];
        }
        return path;
    }

    function _str(i) { return parseInt(i, 10) }

    /*
    * Translate a list of points ([[x1, y1], [x2, y2] ...]) to a standard
    * SVG Path String.
    * TODO: rigorous error handling. Should this be done at the frontend or backend?
    */
    function _toPathString(orig_list) {
        var str = "";
        var chunk = 2;
        var list = new Array(orig_list.length);

        // transform all points to screen space
        for(var i = 0; i < list.length; i++)
            list[i] = _toScreen(orig_list[i]);

        // First point is the starting point
        str += "M " + _str(list[0][0]) + " " + _str(list[0][1]) + " ";

        // if we have only two points, simply draw a line
        if(list.length == 2) {
            str += "L " + _str(list[1][0]) + " " + _str(list[1][1]) + " ";
            return str;
        } else {
            // TODO: document this properly
            return catmullRomSpline(list, 1)
        }

        // TODO: does not work for curve with 3 points
        // NOTE: the following code treats points from Runtime as control
        // points for a Cubic Bezier Curve, which will NOT pass through
        // the control points

        // Second through fourth points are for the first Bezier Curve
        // str += "C " + list[1][0] + " " + list[1][1] + ", " +
        //               list[2][0] + " " + list[2][1] + ", " +
        //               list[3][0] + " " + list[3][1] + " ";
        // for(var i = 4; i < list.length; i += chunk) {
        //     points = list.slice(i, i + chunk);
        //     str += "S "
        //     for(var j = 0; j < points.length; j++) {
        //         str += points[j][0] + " " + points[j][1] + ", ";
        //     }
        // }
        // return str.substring(0, str.length - 2);
    }

    function _allToScreen(orig_list, dx, dy) {
        var list = new Array(orig_list.length);
        // transform all points to screen space
        for(var i = 0; i < list.length; i++) {
            list[i] = _toScreen(orig_list[i], dx, dy);
        }
        return list;
    }

     function _toScreen(xy) {
         return [CANVAS_WIDTH/2  + xy[0],
                 CANVAS_HEIGHT/2 - xy[1]]
     }

     return {
         scr: _toScreen,
         allToScreen: _allToScreen,
         hex: _rgbToHex,
         path_str: _toPathString
     }
})(); // end of Utils module

////////////////////////////////////////////////////////////////////////////////

function main() {
    var s = Snap("#svgdiv");
    $("#svgdiv").css("width",  CANVAS_WIDTH);
    $("#svgdiv").css("height", CANVAS_HEIGHT);

    var firstRun = true
    ws = new WebSocket(socketAddress);
    // Only sample in 10 ms intervals, regardless of Penrose's sample speed
    var lastTime = new Date().getTime()
    ws.onopen = function() {
        // Register handlers for buttons on connection
        $("#resample").click(function() {
            var dict = { "tag" : "Cmd", "contents" : { "command" : "resample" } }
            var json = JSON.stringify(dict)
            ws.send(json)
        });
        $("#step").click(function() {
            var dict = { "tag" : "Cmd", "contents" : { "command" : "step" } }
            var json = JSON.stringify(dict)
            ws.send(json)
        });
        $("#autostep").click(function() {
            var $this = $(this);
            $this.toggleClass('On');
            if($this.hasClass('On')){
                $this.text('Disable Autostep');
            } else {
                $this.text('Enable Autostep');
            }
            var dict = { "tag" : "Cmd", "contents" : {"command" : "autostep" } }
            var json = JSON.stringify(dict)
            ws.send(json)
        });
    };
    ws.onmessage = async function(event) {
        //console.log(event.data)
        var now  = new Date().getTime();
        var diff = (now - lastTime);
        var json = jQuery.parseJSON(event.data);

        // the server only sends `Frame` type data for the __last__ frame
        if(json.flag == "final") {
            Render.scene(ws, s, json.shapes, label_svgs, firstRun);
        } else {
            var objs = json
            // if not the last frame, we refresh the frontend on a time interval
            if(firstRun) {
                label_svgs = await Render.collectLabels(objs)
                if(DEBUG) {
                    console.log("Generated labels:")
                    console.log(label_svgs)
                }
            }
            if(firstRun || diff > SAMPLE_INTERVAL) {
                Render.scene(ws, s, objs, label_svgs, firstRun);
                lastTime = now;
                firstRun = false;
            }
        }
    }
} // end of main function

// Main function invokation
$(document).ready(function () {
    $.getScript('snap.svg.js', main);
});
