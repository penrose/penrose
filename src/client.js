 $.getScript('snap.svg.js', function()
 {

    // Main rendering function
    function renderScene(ws, s, data, firstrun) {
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
            }
        }
        // Send the bbox information to the server
        if(firstrun) {
            var dict = { "tag" : "Update", "contents" : { "objs" : data } }
            var json = JSON.stringify(dict)
            ws.send(json)
        }
    }

});
