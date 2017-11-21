/*
 * Penrose web front-end main script
 * @author: Wode "Nimo" Ni
 * @version: 06/23/2017
 */


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

    /*
     * Translate a list of points ([[x1, y1], [x2, y2] ...]) to a standard
     * SVG Path String.
     * TODO: rigorous error handling. Should this be done at the frontend or backend?
     */
    function toPathString(orig_list, dx, dy) {
        var str = "";
        var chunk = 2;
        var list = new Array(orig_list.length);

        // console.log("haha")
        // console.log(orig_list)
        // transform all points to screen space
        for(var i = 0; i < list.length; i++) {
            list[i] = toScreen(orig_list[i], dx, dy);
        }
        // console.log("wawa")
        // console.log(list)
        // list = orig_list
        // First point is the starting point
        str += "M " + list[0][0] + " " + list[0][1] + " ";
        // if we have only two points, simply draw a line
        if(list.length == 2) {
            str += "L " + list[1][0] + " " + list[1][1] + " ";
            return str;
        }
	// TODO: does not work for curve with 3 points
        // Second through fourth points are for the first Bezier Curve
        str += "C " + list[1][0] + " " + list[1][1] + ", " +
                      list[2][0] + " " + list[2][1] + ", " +
                      list[3][0] + " " + list[3][1] + " ";
        for(var i = 4; i < list.length; i += chunk) {
            points = list.slice(i, i + chunk);
            str += "S "
            for(var j = 0; j < points.length; j++) {
                str += points[j][0] + " " + points[j][1] + ", ";
            }
        }
        // console.log(str.substring(0, str.length - 2));
        return str.substring(0, str.length - 2);
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
        var dx = s.node.clientWidth  / 2
        var dy = s.node.clientHeight / 2

        for (var key in data) {
            // console.log(data[key])
            var record = data[key]
            var obj = record.contents
            switch(record.tag) {
                case 'CB': // cubic bezier
                    // var curve = s.path(toPathString(obj.pathcb, dx, dy));
		    var curve = s.polyline(allToScreen(obj.pathcb, dx, dy));
                    curve.data("name", obj.namecb)
                    var color = obj.colorcb;
                    // by default, the curve should be solid
                    curve.attr({
                        fill: "transparent",
                        strokeWidth: 5,
                        stroke: rgbToHex(color.r, color.g, color.b)
                    });
                    if(obj.stylecb == "dashed") {
                        curve.attr({
                            strokeDasharray: "10"
                        });
                    }
                    curve.drag(move, start, stop)
                break
                case 'L': // label
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
                case 'A': // arrow
                    var sx = dx + obj.startx, sy = dy - obj.starty,
                        ex = dx + obj.endx,   ey = dy - obj.endy,
                        t  = obj.thickness / 6
                    var len = Snap.len(ex, ey, sx, sy)
                    var body_path = [0, 0 + t, len - 5*t, t, len - 5*t, -1*t, 0, -1*t]
                    var head_path = [len - 5*t, 3*t, len, 0, len - 5*t, -3*t]
                    var angle = Snap.angle(ex, ey, sx, sy)
                    var myMatrix = new Snap.Matrix();
                    myMatrix.translate(sx, sy);
                    myMatrix.rotate(angle, 0, 0);
                    var line = s.polygon(body_path).transform(myMatrix.toTransformString())
                    var head = s.polygon(head_path).transform(myMatrix.toTransformString())
                    var g = s.g(line, head)
                    g.data("name", obj.namesa)
                    g.drag(move, start, stop)
                    // // render the bbox
                    // var sq = s.path(g.getBBox().path);
                    // sq.attr({
                    //     "fill-opacity": 0,
                    //     stroke: "#000",
                    //     strokeWidth: 1, // CamelCase...
                    // })
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
            // console.log(event.data)
            var now  = new Date().getTime()
            var diff = (now - lastTime);
            if(firstRun || diff > sampleInterval) {
                var obj = jQuery.parseJSON(event.data)
                // console.log(obj)
                renderScene(ws, s, obj, firstRun)
                lastTime = now
                firstRun = false
            }
        }
    });
});
