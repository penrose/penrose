/*
 * Penrose web front-end main script
 * @author: Wode "Nimo" Ni
 * @version: 06/23/2017
 */

var canvasWidth  = 800
var canvasHeight = 700

 $.getScript('snap.svg.js', function()
 {
    // Helper functions

    function createSocket() {
        return new WebSocket('ws://localhost:9160/');
    }

    // For (r,g,b) -> hex conversion
    function componentToHex(c) {
        var hex = c.toString(16);
        return hex.length == 1 ? "0" + hex : hex;
    }

    function rgbToHex(r, g, b) {
        return "#" + componentToHex(Math.round(255 * r)) + componentToHex(Math.round(255 * g)) + componentToHex(Math.round(255 * b));
    }

    function renderPoints(canvas, point_list, dx, dy) {
        for(var i = 0; i < point_list.length; i++) {
            var xy = toScreen(point_list[i], dx, dy);
            var point = canvas.circle(xy[0], xy[1], 5);
            point.attr({
                fill: "none"
            })
        }
    }

    /**
    Calculate Tangence from degrees
    **/
    function getTanFromDegrees(degrees) {
        return Math.tan(degrees * Math.PI/180);
    }


        /*
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
     * Given center point, radius, and angels return an arc path
     * Adopted from: https://codepen.io/AnotherLinuxUser/pen/QEJmkN
     * (Added by Dor)
     * Contaikns only the arc
     */
     function describeArc(x, y, radius, startAngle, endAngle) {
        var start = polarToCartesian(x, y, radius, endAngle);
        var end = polarToCartesian(x, y, radius, startAngle);
        var arcSweep = endAngle - startAngle <= 180 ? "0" : "1";
        return [
        "M", start.x, start.y,
        "A", radius, radius, 0, arcSweep, 0, end.x, end.y
        ].join(" ");
    }

    /*
     * Given center point, radius, and angels return an arc path
     * Adopted from: https://codepen.io/AnotherLinuxUser/pen/QEJmkN
     * (Added by Dor)
     * Returns the full "pie wedge" path
     */
    function describeFullArc(x, y, radius, startAngle, endAngle) {
        var start = polarToCartesian(x, y, radius, endAngle);
        var end = polarToCartesian(x, y, radius, startAngle);
        var arcSweep = endAngle - startAngle <= 180 ? "0" : "1";
        return [
        "M", start.x, start.y,
        "A", radius, radius, 0, arcSweep, 0, end.x, end.y,
        "L", x, y,
        "L", start.x, start.y
        ].join(" ");
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

    /*
     * Translate a list of points ([[x1, y1], [x2, y2] ...]) to a standard
     * SVG Path String.
     * TODO: rigorous error handling. Should this be done at the frontend or backend?
     */
     function toPathString(orig_list, dx, dy) {
        var str = "";
        var chunk = 2;
        var list = new Array(orig_list.length);

        // transform all points to screen space
        for(var i = 0; i < list.length; i++) {
            list[i] = toScreen(orig_list[i], dx, dy);
        }
        // First point is the starting point
        str += "M " + list[0][0] + " " + list[0][1] + " ";
        // if we have only two points, simply draw a line
        if(list.length == 2) {
            str += "L " + list[1][0] + " " + list[1][1] + " ";
            return str;
        } else {
            var res = catmullRomSpline(list, 1)
            return res
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


    /*
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
      * Given center point, radius, and angels return an arc path
      * Adopted from: https://codepen.io/AnotherLinuxUser/pen/QEJmkN
      * (Added by Dor)
      * Contaikns only the arc
      */
      function describeArc(x, y, radius, startAngle, endAngle) {
       var start = polarToCartesian(x, y, radius, endAngle);
       var end = polarToCartesian(x, y, radius, startAngle);
       var arcSweep = endAngle - startAngle <= 180 ? "0" : "1";
       return [
       "M", start.x, start.y,
       "A", radius, radius, 0, arcSweep, 0, end.x, end.y
       ].join(" ");
   }

   function allToScreen(orig_list, dx, dy) {
    var list = new Array(orig_list.length);

        // transform all points to screen space
        for(var i = 0; i < list.length; i++) {
            list[i] = toScreen(orig_list[i], dx, dy);
        }

        return list;
    }

    function toScreen(xy, dx, dy) {
        return [parseInt(dx + xy[0], 10), parseInt(dy - xy[1], 10)]
    }

    // Main rendering function
    function renderScene(ws, s, data, firstrun) {
        // Handlers for dragging events
        var move = function(dx, dy) {
            this.attr({
                transform: this.data('origTransform') + (this.data('origTransform') ? "T" : "t") + [dx, dy]
            });
            // Increment distance traveled
            this.data("ox", +dx );
            this.data("oy", +dy );
            // console.log(dx + " " + dy)
        }

        var start = function(dx, dy) {
            this.data('origTransform', this.transform().local );
            this.data("ox", 0);
            this.data("oy", 0);
            this.attr({opacity: 0.5});
        }
        var stop = function() {
            // console.log('finished dragging');
            // console.log('distance: ' + this.data("ox") + " " + this.data("oy"));
            this.attr({opacity: 1});
            var dict = { "tag" : "Drag",
            "contents" : { "name" : this.data("name"),
            "xm" : this.data("ox"),
            "ym" : this.data("oy")} }
            var json = JSON.stringify(dict)
            // console.log(json)
            ws.send(json)
        }
        s.clear()
        // NOTE: just using clientWidth/Height does not work on Firefox
        // see https://stackoverflow.com/questions/13122790/how-to-get-svg-element-dimensions-in-firefox
        var dx = canvasWidth  / 2
        var dy = canvasHeight / 2

        for (var key in data) {
            // console.log(data[key])
            var record = data[key]
            var obj = record.contents
            switch(record.tag) {
                case 'CB': // cubic bezier
                var curve = s.path(toPathString(obj.pathcb, dx, dy));
                curve.data("name", obj.namecb)
                var color = obj.colorcb;
                    // by default, the curve should be solid
                    curve.attr({
                        fill: "transparent",
                        strokeWidth: 2.5, // this should be settable in Style
                        stroke: rgbToHex(color.r, color.g, color.b)
                    });
                    if(obj.stylecb == "dashed") {
                        curve.attr({
                            strokeDasharray: "7, 5" // "10"
                        });
                    }
                    curve.drag(move, start, stop)
                    // DEBUG: showing control points and poly line
                    debug_bezier = false;
                    if (debug_bezier) {
                        var polyLine = s.polyline(allToScreen(obj.pathcb, dx, dy));
                        var controlPts = renderPoints(s, obj.pathcb, dx, dy);
                        polyLine.attr({
                            fill: "transparent",
                            strokeWidth: 5,
                            stroke: rgbToHex(color.r, color.g, color.b),
                            strokeDasharray: "10"
                        });
                    }
                    break

                case 'LN': // line (modified from arrow/bezier)
                var path = [[obj.startx_l, obj.starty_l], [obj.endx_l, obj.endy_l]];
                var curve = s.path(toPathString(path, dx, dy));
                curve.data("name", obj.name_l)
                var color = obj.color_l;
                    // by default, the curve should be solid
                    curve.attr({
                        fill: "transparent",
                        strokeWidth: obj.thickness_l,
                        stroke: rgbToHex(color.r, color.g, color.b)
                    });
                    if(obj.style_l == "dashed") {
                        curve.attr({
                            strokeDasharray: "7, 5" // "10"
                        });
                    }
                    curve.drag(move, start, stop)
                    break
           // var sx = dx + obj.startx_l, sy = dy - obj.starty_l,
                   //      ex = dx + obj.endx_l,   ey = dy - obj.endy_l,
                   //      t  = obj.thickness_l / 12
                   //  var len = Snap.len(ex, ey, sx, sy)
                   //  var body_path = [0, 0 + t, len - 5*t, t, len - 5*t, -1*t, 0, -1*t]

                   //  var angle = Snap.angle(ex, ey, sx, sy)
                   //  var myMatrix = new Snap.Matrix();
                   //  myMatrix.translate(sx, sy);
                   //  myMatrix.rotate(angle, 0, 0);
                   //  var line = s.polygon(body_path).transform(myMatrix.toTransformString())

                   //  var g = s.g(line)
                   //  g.data("name", obj.name_l)
                   //  g.drag(move, start, stop)


                case 'L': // label (TODO: don't display Text shape? need to distinguish b/t text and label)
                var t = s.text(dx + obj.xl, dy - obj.yl, [obj.textl]);
                t.data("name", obj.namel)
                t.attr({
                    "font-style": "italic",
                    "font-family": "Palatino"
                });
                var bbox = t.getBBox()
                var mat = new Snap.Matrix()
                    // Fix the center of labels
                    mat.translate(bbox.width / -2, bbox.height / 2)
                    t.transform(mat.toTransformString())
                    t.drag(move, start, stop)
                    if(firstrun) {
                        obj.wl = bbox.width
                        obj.hl = bbox.height
                    }
                    // // render the bbox
                    // var sq = s.path(t.getBBox().path);
                    // sq.attr({
                    //     "fill-opacity": 0,
                    //     stroke: "#000",
                    //     strokeWidth: 1, // CamelCase...
                    // })
                    // var g = s.g(sq, t)
                    // // render Bounding circle
                    // var circ = s.circle(dx + obj.xl, dy - obj.yl, t.getBBox().r0)
                    // circ.attr({
                    //     "fill-opacity": 0,
                    //     stroke: "#000",
                    //     strokeWidth: 1, // CamelCase...
                    // })
                    // circ.drag()
                    break
                case 'P': // point
                var pt = s.circle(dx + obj.xp, dy - obj.yp, 4);
                pt.data("name", obj.namep)
                var color = obj.colorp
                pt.attr({
                    fill: "#000000",
                    "fill-opacity": 1
                });
                pt.drag(move, start, stop)
                break
                case 'C': // circle
                var circ = s.circle(dx + obj.xc, dy - obj.yc, obj.r);
                circ.data("name", obj.namec)
                var color = obj.colorc
                circ.attr({
                    fill: rgbToHex(color.r, color.g, color.b),
                    "fill-opacity": color.a,
                });
                circ.drag(move, start, stop)
                break
                case 'E': // ellipse
                var ellip = s.ellipse(dx + obj.xe, dy - obj.ye, obj.rx, obj.ry);
                ellip.data("name", obj.namee)
                var color = obj.colore
                ellip.attr({
                    fill: rgbToHex(color.r, color.g, color.b),
                    "fill-opacity": color.a,
                });
                ellip.drag(move, start, stop)
                break
                case 'S': // square
                var side = obj.side
                var sq = s.rect(dx + obj.xs - side/2, dy - obj.ys - side/2, side, side);
                sq.data("name", obj.names)
                var color = obj.colors
                sq.attr({
                    fill: rgbToHex(color.r, color.g, color.b),
                    "fill-opacity": color.a,
                });
                sq.drag(move, start, stop)
                break

                case 'AR': // Angle Mark
                var isRightar = obj.isRightar
                var sizear = obj.sizear
                var color = obj.colorar
                var xar = obj.xar
                var yar = obj.yar
                var style = obj.stylear
                var rotationar = obj.rotationar

                if(isRightar == "true"){
                        //Draw PrepMark (for right angle)                        

                        var myMatrix = new Snap.Matrix();
                        var origX = dx + xar, origY = dy + yar
                        
                        myMatrix.translate(origX, origY);
                        myMatrix.rotate(rotationar, 0, 0);
                        
                        var path = "M " + 0 + " " + (-sizear)+ " L "
                        + sizear + " " +  (-sizear) + " L " 
                        + sizear + " " + yar
                        var p = s.path(path).transform(myMatrix.toTransformString())

                        p.attr({
                            fill: rgbToHex(color.r, color.g, color.b),
                            "fill-opacity": 0,
                            stroke: rgbToHex(color.r, color.g, color.b),
                            strokeWidth: 2
                        });
                        if(style == "line"){
                            p.data("name", obj.namear)
                            p.drag(move, start, stop)

                        } else if (style == "wedge") {
                            
                            var rectPpath = "M " + 0 + " " + (-sizear)+ " L "
                            + sizear + " " +  (-sizear) + " L " 
                            + sizear + " " + 0 + " L " + 0 + " " + 0
                            
                            var f = s.path(rectPpath).transform(myMatrix.toTransformString());
                            f.attr({
                                fill: rgbToHex(0, 0, 205),
                                "fill-opacity": 0.6,
                            });
                            var g = s.g(p,f)
                            g.data("name", obj.namear)
                            g.drag(move, start, stop)
                        }
                    }

                    if(isRightar == "false"){
                        //Draw arc (for regular angles)
                        var anglear = obj.anglear > 360.0 ? 360.0 - obj.anglear : obj.anglear
                        anglear = anglear < 0 ? 360 + anglear : anglear
                        var radiusar = obj.radiusar
                        var arcPath = describeArc(dx + xar, dy + yar, radiusar, rotationar, 
                            anglear < 0 ? (rotationar - anglear) : (rotationar + anglear));                          
                        var arc = s.path(arcPath);
                        arc.attr({
                            "fill-opacity": 0,
                            stroke: rgbToHex(color.r, color.g, color.b),
                            strokeWidth: 2
                        });
                        if(style == "line"){
                            arc.data("name", obj.namear)
                            arc.drag(move, start, stop)
                        } else if (style == "wedge") {
                            var pf = describeFullArc(dx + xar, dy + yar, radiusar, rotationar, 
                                anglear < 0 ? (rotationar - anglear) : (rotationar + anglear));                          
                            var f = s.path(pf);
                            f.attr({
                                fill: rgbToHex(0, 0, 205),
                                "fill-opacity": 0.6,
                            });
                            var g = s.g(arc,f)
                            g.data("name", obj.namear)
                            g.drag(move, start, stop)
                        }

                    }
                    break
                case 'R': // rectangle
        // TODO fix this!
        var sizeX = obj.sizeX;
        var sizeY = obj.sizeY;
        var rect = s.rect(dx + obj.xr - sizeX/2, dy - obj.yr - sizeY/2, sizeX, sizeY);
        // isn't this bottom left?
        rect.data("name", obj.namer)
        var color = obj.colorr;
        rect.attr({
            fill: rgbToHex(color.r, color.g, color.b),
            "fill-opacity": color.a,
        });
        rect.drag(move, start, stop)
        break

        case 'PA': // parallelogram
        // TODO fix this!
        var sx = dx + obj.xpa, sy = dy - obj.ypa
        var sizeX = obj.sizeXpa;
        var sizeY = obj.sizeYpa;
        var angle = obj.anglepa;
        var rotation = obj.rotationpa
        var path = ""
        if(angle <= 90.0){
          path = "M " + (-(sizeX/2) - sizeY/getTanFromDegrees(angle)) + " " + (-(sizeY/2)) +  " L " + 
          (-(sizeX/2)) + " " + (sizeY/2) +  " L " +  (sizeX/2) + " " + (sizeY/2) +  " L " +
          ((sizeX/2) - (sizeY/getTanFromDegrees(angle))) + " " + (-(sizeY/2))
      }
      else{
          path = "M " + (-(sizeX/2)) + " " + (sizeY/2) +  " L " + 
          (sizeX/2) + " " + (sizeY/2) +  " L " + ((sizeX/2) + sizeY/getTanFromDegrees(angle)) + " " + (-(sizeY/2))
          +  " L " + (-(sizeX/2) + (sizeY/getTanFromDegrees(angle))) + " " + (-(sizeY/2))
      }

      var myMatrix = new Snap.Matrix();
      myMatrix.translate(sx, sy);
      myMatrix.rotate(rotation, 0, 0);
      var parallelogram = s.path(path).transform(myMatrix.toTransformString())
      parallelogram.data("name", obj.namepa)
      var color = obj.colorpa;
      parallelogram.attr({

        fill: rgbToHex(color.r, color.g, color.b),
        "fill-opacity": color.a,
    });
      parallelogram.drag(move, start, stop)
      break

                case 'A': // arrow
                var style = obj.stylesa
                var sx = dx + obj.startx, sy = dy - obj.starty,
                ex = dx + obj.endx,   ey = dy - obj.endy,
                t  = obj.thickness / 6
                var len = Snap.len(ex, ey, sx, sy)
                var body_path = [0, t, len - 5*t, t, len - 5*t, -1*t, 0, -1*t]
                var head_path = [len - 5*t, 3*t, len, 0, len - 5*t, -3*t]
                var angle = Snap.angle(ex, ey, sx, sy)
                var myMatrix = new Snap.Matrix();
                myMatrix.translate(sx , sy);
                myMatrix.rotate(angle, 0, 0);
                console.log(obj.namesa + " ex: " + ex + ", ey: " + ey + " angle: " + angle + "\n");
                var color = obj.colorsa
                if(style == "straight"){
                    var line = s.polygon(body_path).transform(myMatrix.toTransformString())
                }                    
                if(style == "curved"){
                    line = s.path(describeArc(len/2-2.4*t,0,len/2,-90,90)).transform(myMatrix.toTransformString())
                    line.attr({
                        "fill-opacity" : 0,
                        stroke: rgbToHex(color.r, color.g, color.b),
                        strokeWidth: 2
                    })
                }
                
                if(style == "length"){
                    //document.write(toPathString([[0,(-(4*t))],[0,(4*t)]],0,0))
                    var p = "M " + 0 + " " + (-(4*t)) + " L " + 0 + " " + (4*t) +  " L " + 0 + " " + 0 + " L " + (len-(5*t)) + " " + 0;
                    var tail1 = s.path(p)
                        .transform(myMatrix.toTransformString())
                    tail1.attr({
                        "fill-opacity" : 0,
                        stroke: rgbToHex(color.r, color.g, color.b),
                        strokeWidth: 2
                    })
                    var head1 = s.path(toPathString([[(len-(5*t)),(-(4*t))],[len-(5*t),(4*t)]],0,0)).transform(myMatrix.toTransformString())
                    head1.attr({
                        "fill-opacity" : 0,
                        stroke: rgbToHex(color.r, color.g, color.b),
                        strokeWidth: 2
                    })
                    var g1 = s.g(head1, tail1)
                    g1.data("name", obj.namesa)
                    g1.drag(move, start, stop)
                }
                else{
                    var head = s.polyline(head_path).transform(myMatrix.toTransformString())
                    var g = s.g(line, head)
                    g.data("name", obj.namesa)
                    g.drag(move, start, stop)
                }
                break
                //Case image
                case 'IM':
                    var sizeX = obj.sizeXim
                    var sizeY = obj.sizeYim
                    var xim = obj.xim
                    var yim = obj.yim
                    var path = obj.path

                    var image = s.image(path, dx + obj.xim - sizeX/2, dy - obj.yim - sizeY/2, sizeX, sizeY)
                    image.data("name", obj.nameim)
                    image.drag(move, start, stop)

                break

            }
        }
        // Send the bbox information to the server
        if(firstrun) {
            var dict = { "tag" : "Update", "contents" : { "objs" : data } }
            var json = JSON.stringify(dict)
            ws.send(json)
        }
    }

    // Main function
    $(document).ready(function () {
        // var s = Snap(800, 700);
        // TODO: set the width and height here?
        var s = Snap("#svgdiv");
        $("#svgdiv").css("width", canvasWidth);
        $("#svgdiv").css("height", canvasHeight);

        var firstRun = true
        ws = createSocket();
        // Only sample in 10 ms intervals, regardless of Penrose's sample speed
        var sampleInterval = 20
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
        ws.onmessage = function(event) {
            //console.log(event.data)
            var now  = new Date().getTime()
            var diff = (now - lastTime);
            var obj = jQuery.parseJSON(event.data)

            // the server only sends `Frame` type data for the __last__ frame
            if(obj.flag == "final") {
                renderScene(ws, s, obj.objs, firstRun)
            } else {
                // if not the last frame, we refresh the frontend on a time interval
                if(firstRun || diff > sampleInterval) {
                   renderScene(ws, s, obj, firstRun)
                    lastTime = now
                    firstRun = false
                }
            }
        }
    });
});