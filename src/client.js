$.getScript('snap.svg.js', function()
{
    function createSocket() {
        return new WebSocket('ws://localhost:9160/');
    }

    function componentToHex(c) {
        var hex = c.toString(16);
        return hex.length == 1 ? "0" + hex : hex;
    }

    function rgbToHex(r, g, b) {
        return "#" + componentToHex(Math.round(255 * r)) + componentToHex(Math.round(255 * g)) + componentToHex(Math.round(255 * b));
    }


    function renderScene(s, data) {
        s.clear()
        var dx = 800 / 2
        var dy = 700 / 2
        for (var key in data) {
            // console.log(data[key])
            var record = data[key]
            var obj = record.contents
            switch(record.tag) {
                case 'L': // label
                    // console.log("label!")
                    console.log(obj.namel)
                    // Provide an array of strings (or arrays), to generate tspans
                    var t = s.text(dx + obj.xl, dy - obj.yl, [obj.textl]);
                    t.drag()
                    // t.selectAll("tspan:nth-child(3)").attr({
                    //     fill: "#900",
                    //     "font-size": "20px"
                    // });
                break
                case 'C': // label
                    var circ = s.circle(dx + obj.xc, dy - obj.yc, obj.r);
                    var color = obj.colorc
                    console.log(rgbToHex(color.r, color.g, color.b))
                    circ.attr({
                        fill: rgbToHex(color.r, color.g, color.b),
                        "fill-opacity": color.a,
                    });
                    // console.log("circle!")
                    // console.log(obj.namec)
                    circ.drag()
                    console.log(obj.xc)
                break
                case 'S': // label
                    var side = obj.side
                    var sq = s.rect(dx + obj.xs - side/2, dy - obj.ys - side/2, side, side);
                    var color = obj.colors
                    console.log(rgbToHex(color.r, color.g, color.b))
                    sq.attr({
                        fill: rgbToHex(color.r, color.g, color.b),
                        "fill-opacity": color.a,
                    });
                    sq.drag()
                    // console.log("Square!")
                    console.log(obj.names)
                break
                case 'A': // label
                    // console.log("SolidArrow!")
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
                    console.log(obj.namesa)
                    var g = s.g(line, head)
                    g.drag()
                break
            }
        }
    }

    $(document).ready(function () {
        var s = Snap(800, 700);
        var ws = createSocket();
        var sampleInterval = 10
        var lastTime = new Date().getTime()
        ws.onopen = function() {
            // ws.send('Hi! I am ' + user);
            // alert("Hello, world!")
        };
        ws.onmessage = function(event) {
            // console.log(event.data)
            // document.write(event.data)
            var now  = new Date().getTime()
            var diff = (now - lastTime);
            if(diff > sampleInterval) {
                var obj = jQuery.parseJSON(event.data)
                renderScene(s, obj)
                lastTime = now
            }
        }
    });

});
