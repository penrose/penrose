/*
 * Penrose web front-end main script
 * @author: Wode "Nimo" Ni
 * @version: 06/23/2017
 */


$.getScript('snap.svg.js', function()
{
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



    // Main rendering function
    function renderScene(ws, s, data) {
        // Handlers for dragging events
        var move = function(dx,dy) {
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
            // record the current positions
            this.data("ox", +this.getBBox().cx );
            this.data("oy", +this.getBBox().cy );
            this.attr({opacity: 0.5});
        }
        var stop = function() {
            // console.log('finished dragging');
            // console.log('distance: ' + this.data("ox") + " " + this.data("oy"));
            this.attr({opacity: 1});
            var dict = { "name" : this.data("name"), "xm" : this.data("ox"), "ym" : this.data("oy")}
            var json = JSON.stringify(dict)
            // console.log(json)
            ws.send(json)
        }
        s.clear()
        var dx = 800 / 2
        var dy = 700 / 2
        for (var key in data) {
            // console.log(data[key])
            var record = data[key]
            var obj = record.contents
            switch(record.tag) {
                case 'L': // label
                    var t = s.text(dx + obj.xl, dy - obj.yl, [obj.textl]);
                    t.data("name", obj.namel)
                    var bbox = t.getBBox()
                    var mat = new Snap.Matrix()
                    // Fix the center of labels
                    mat.translate(bbox.width / -2, bbox.height / 2)
                    t.transform(mat.toTransformString())
                    t.drag(move, start, stop)
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
                break
            }
        }
    }

    $(document).ready(function () {
        // var s = Snap(800, 700);
        // TODO: set the width and height here?
        var s = Snap("#svgdiv");
        ws = createSocket();
        // Only sample in 10 ms intervals, regardless of Penrose's sample speed
        var sampleInterval = 20
        var lastTime = new Date().getTime()
        ws.onopen = function() {
            // Register handlers for buttons on connection
            $("#resample").click(function() {
                var dict = { "command" : "resample" }
                var json = JSON.stringify(dict)
                ws.send(json)
            });
            $("#step").click(function() {
                var dict = { "command" : "step" }
                var json = JSON.stringify(dict)
                ws.send(json)
            });
            $("#autostep").click(function() {
                // console.log("autostep")
                var $this = $(this);
                $this.toggleClass('On');
                if($this.hasClass('On')){
                    $this.text('Disable Autostep');
                } else {
                    $this.text('Enable Autostep');
                }
                var dict = { "command" : "autostep" }
                var json = JSON.stringify(dict)
                ws.send(json)
            });
        };
        ws.onmessage = function(event) {
            // console.log(event.data)
            // document.write(event.data)
            var now  = new Date().getTime()
            var diff = (now - lastTime);
            if(diff > sampleInterval) {
                var obj = jQuery.parseJSON(event.data)
                // console.log(obj)
                renderScene(ws, s, obj)
                lastTime = now
            }
        }
    });
});
