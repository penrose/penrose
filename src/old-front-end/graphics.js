// NOT PORTED denotes functions that needs to be ported to the new Penrose system

var Render = (function(){


  /**
  Calculate sine from degrees
  **/
  function getSinFromDegrees(degrees) {
    return Math.sin(degrees * Math.PI/180);
  }

  /**
  Calculate cosine from degrees
  **/
  function getCosFromDegrees(degrees) {
    return Math.cos(degrees * Math.PI/180);
  }

  /**
  * [tex2svg description]
  * @param  {[type]}   texstring [description]
  * @param  {[type]}   id        to be used to insert into dictionary
  * @param  {Function} callback  [description]
  * @return {[type]}             [description]
  */
  function tex2svg(texstring, id) {
    return new Promise(function(resolve, reject){
      var input = texstring;
      var wrapper = document.createElement("div");
      wrapper.innerHTML = input;
      MathJax.Hub.Queue(["Typeset", MathJax.Hub, wrapper]);
      MathJax.Hub.Queue(function() {
        var mjOut = wrapper.getElementsByTagName("svg")[0];
        mjOut.setAttribute("xmlns", "http://www.w3.org/2000/svg");
        svg = mjOut.outerHTML;
        // FIXME: delete wrapper?
        resolve({ "name": id, "svg": svg });
        // TODO: reject??
      });
    })
  }

  /**
  * handler for dragging event, called in the middle of dragging event
  * @param  {float} dx movement along x axis
  * @param  {float} dy movement along y axis
  */
  var move = function(dx, dy) {
    this.attr({
      transform: this.data('origTransform') + (this.data('origTransform') ? "T" : "t") + [dx, dy]
    });
    // Increment distance traveled
    this.data("ox", dx);
    this.data("oy", -dy);
  }

  /**
  * handler for dragging event, called at the start of dragging event
  * @param  {float} dx NOT USED
  * @param  {float} dy NOT USED
  */
  var start = function(dx, dy) {
    this.data('origTransform', this.transform().local );
    this.data("ox", 0);
    this.data("oy", 0);
    this.attr({opacity: 0.5});
  }

  /**
  * handler for dragging event, called at the end of dragging event
  * @param  {float} dx NOT USED
  * @param  {float} dy NOT USED
  */
  var stop = function() {
    if(DEBUG) {
      console.log('finished dragging: ');
      console.log('distance: ' + this.data("ox") + " " + this.data("oy"));
    }
    this.attr({opacity: 1});
    var dict = { "tag" : "Drag",
    "contents" : {
      "name" : this.data("name"),
      "xm" : this.data("ox"),
      "ym" : this.data("oy")}
    }
    var json = JSON.stringify(dict)
    if(DEBUG) {
      console.log("Updating dragging event to server: ")
      console.log(json)
    }
    ws.send(json)
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
  * Given center point, radius, and angels return an filled arc
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
  * Given center point, radius, and angels return an arc path
  * Adopted from: https://codepen.io/AnotherLinuxUser/pen/QEJmkN
  * (Added by Dor)
  * Contains only the arc, for use in angle marks and for curved arrows
  */
  function describeArc(x, y, radius, startAngle, endAngle,isElliptical) {
    var start = polarToCartesian(x, y, radius, endAngle);
    var end = polarToCartesian(x, y, radius, startAngle);
    var arcSweep = endAngle - startAngle <= 180 ? "0" : "1";
    return [
      "M", start.x, start.y,
      "A", radius, (isElliptical ? radius/2 : radius) , 0, arcSweep, 0, end.x, end.y
    ].join(" ");
  }

  /**
  * FIXME: refactor
  */
  function _renderPoints(canvas, point_list) {
    var res = []
    for(var i = 0; i < point_list.length; i++) {
      var xy = Utils.scr(point_list[i]);
      var point = canvas.circle(xy[0], xy[1], 5);
      point.attr({
        fill: "none"
      })
      // res.push(point)
    }
    // return canvas.g(res)
  }

  /**
  * Top-level function that renders a frame of Penrose diagram
  * @param       {WebSocket} ws    WebSocket connection
  * @param       {Snap} s          Snap.svg canvas
  * @param       {JSON} data       Scene data from Haskell server
  * @param       {Boolean} firstrun flag indicating whether is this the first frame
  */
  function _renderScene(ws, s, shapes, labels, firstrun) {
    s.clear()
    // NOTE: just using clientWidth/Height does not work on Firefox
    // see https://stackoverflow.com/questions/13122790/how-to-get-svg-element-dimensions-in-firefox
    if(DEBUG) {
      console.log("Incoming GPIs from server: ")
      console.log(shapes)
    }
    for (var key in shapes) {
      // retrieve a shape and its associated properties
      var shape = shapes[key]
      var props = {}
      var type  = shape[0]

      // strip off all types of properties
      for(var i in shape[1]) { props[i] = shape[1][i].contents }

      // transform coordinates to screen space
      [screen_x, screen_y] = Utils.scr([props.x, props.y])
      props.x = screen_x; props.y = screen_y

      var renderedShape = null
      switch(type) {
        case 'Circle'        :
        renderedShape = _renderCircle(s, props); break
        case 'Eillipse'      :
        renderedShape = _renderEllipse(s, props); break
        case 'Text'          :
        renderedShape = _renderLabel(s, props, labels, shape[1], firstrun); break
        case 'Point'         :
        renderedShape = _renderPoint(s, props); break
        case 'Rectangle'     :
        renderedShape = _renderRectangle(s, props); break
        case 'Square'        :
        renderedShape = _renderSquare(s, props); break
        case 'Arrow'         :
        renderedShape = _renderArrow (s, props); break
        case 'Brace'         :
        renderedShape = _renderBrace (s, props); break
        case 'Arc'     :
        renderedShape = _renderArc(s, props); break
        case 'Curve'         :
        renderedShape = _renderCurve(s, props); break
        case 'Line'          :
        renderedShape = _renderLine(s, props); break
        case 'Parallelogram' :
        renderedShape = _renderParallelogram(s, props); break
        case 'Image'         :
        renderedShape = _renderImage(s, props); break
        case 'AnchorPoint'   :
        // NOTE: We do not render anchorPts
        // TODO: maybe render it in debug mode
        break
        default: console.log("renderScene: the type of GPI\"" + type + "\" cannot be rendered!")
      }
      console.log("Rendered GPI: ", renderedShape)

      // register name and functions for drag commands
      // NOTE: cases where the shape is not rendered:
      //     - Undefined labels
      if(renderedShape) {
        renderedShape.drag(move, start, stop)
        renderedShape.data("name", props.name)
      }
    }
    // Send the bbox information and label dimensions to the server
    if(firstrun) {
      var dict = { "tag" : "Update", "contents" : { "shapes" : shapes } }
      console.log("Sending updated shapes (e.g. labels with correct bboxes) to the server: ", dict)
      var json = JSON.stringify(dict)
      ws.send(json)
    }
  }

  /**
  * Renders a circle on the canvas
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  */
  function _renderCircle(s, properties) {
    var circ = s.circle(properties.x, properties.y, properties.r);
    var color = properties.color
    var opacity = properties.style == "filled" ? color[3] : 0
    circ.attr({
      fill: Utils.hex(color[0], color[1], color[2]),
      "fill-opacity": opacity,
      "stroke-width": properties["stroke-width"],
      "stroke": "black",
    });
    if(properties["stroke-style"] == "dashed") {
      circ.attr({ strokeDasharray: "7, 5" })
    }
    return circ
  }

  /**
  * Renders an ellipse on the canvas
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  */
  function _renderEllipse(s, properties) {
    [x, y] = [properties.x, properties.y]
    var ellip = s.ellipse(x, y, properties.rx, properties.ry);
    ellip.data("name", properties.name)
    var color = properties.color
    ellip.attr({
      fill: Utils.hex(color[0], color[1], color[2]),
      "fill-opacity": color.a,
    });
    return ellip
  }

  /**
  * Renders an image on the canvas
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  */
  function _renderImage(s, properties) {
    [x, y] = [properties.x, properties.y]
    var sizeX = properties.sizeX
    var sizeY = properties.sizeY
    var path = properties.path
    var image = s.image(path, x, y, sizeX, sizeY)
    return image
  }

  /**
  * Renders a label on the canvas. Note that the labels are pre-generated
  * by the main module separately. This function merely performs a lookup
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  * @param       {JSON} lebels TODO
  * @param       {boolean} firstrun if this is the first run of the server, send back the bbox info
  */
  function _renderLabel(s, properties, labels, shapedef, firstrun) {
    if(labels[properties.name]) {
      [x, y] = [properties.x, properties.y]
      var e = Snap.parse(labels[properties.name])
      t = s.g()
      t.append(e)
      var bbox = t.getBBox()
      var mat = new Snap.Matrix()
      // Fix the center of labels
      mat.translate(x, y)
      mat.translate(-bbox.width/2, -bbox.height/2)
      t.transform(mat.toTransformString())
      if(firstrun) {
        shapedef.w = { tag: "FloatV", contents: bbox.width }
        shapedef.h = { tag: "FloatV", contents: bbox.height }
      }
      if(DEBUG) {
        //  var boundingCircle = _renderBoundingCircle(s, t)
        var boundingBox = _renderBoundingBox(s, t)
        var LabelCenter =  s.circle(x, y, 2)
        return s.g(t, boundingBox, LabelCenter)
      }
      return t
    }
  }

  /**
  * Renders a point on the canvas
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  */
  // FIXME: point radius is hardcoded
  // FIXME: render colored dot
  function _renderPoint(s, properties) {
    [x, y] = [properties.x, properties.y]
    var pt = s.circle(x, y, 4);
    var color = properties.color
    pt.attr({
      fill: "#000000",
      "fill-opacity": 1
    });
    return pt
  }

  /**
  * Renders a square on the canvas
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  */
  function _renderSquare(s, properties) {
    [x, y] = [properties.x, properties.y]
    var side = properties.side
    var sq = s.rect(x - side/2, y - side/2, side, side);
    var color = properties.color
    sq.attr({
      fill: Utils.hex(color[0], color[1], color[2]),
      "fill-opacity": color[3],
      "stroke-width": properties['stroke-width'],
      "stroke": "black"
    });
    if(properties.stylec == "dashed") {
      sq.attr({
        strokeDasharray: "7, 5"
      })
    }
    return sq
  }

  /**
  * Renders a bezier curve on the canvas
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  */
  function _renderCurve(s, properties) {
    var curve = s.path(Utils.path_str(properties.path));
    var color = properties.color;
    // by default, the curve should be solid
    curve.attr({
      fill: "transparent",
      strokeWidth: 2.5, // this should be settable in Style
      stroke: Utils.hex(color[0], color[1], color[2])
    });
    if(properties.stylecb == "dashed") {
      curve.attr({ strokeDasharray: "7, 5" });
    }
    // DEBUG: showing control points and poly line
    if (DEBUG) {
      var polyLine = s.polyline(Utils.allToScreen(properties.path));
      var controlPts = _renderPoints(s, properties.path);
      polyLine.attr({
        fill: "transparent",
        strokeWidth: 5,
        stroke: Utils.hex(color[0], color[1], color[2]),
        strokeDasharray: "10"
      });
      // return s.g(polyLine, controlPts, curve)
      return s.g(polyLine, curve)
    }
    return curve
  }

  /**
  * Renders an arrow on the canvas
  * Arrow can be either a regular or a curved arrow, specified by style
  * @param       {Snap} s  Snap.svg global object
  * @param       {JSON} obj JSON object from Haskell server
  */
  function _renderArrow(s, properties) {
    var style    = properties.style
    var [sx, sy] = Utils.scr([properties.startX, properties.startY])
    var [ex, ey] = Utils.scr([properties.endX, properties.endY])
    var thickness = properties.thickness / 6
    var color = properties.color
    var len = Snap.len(ex, ey, sx, sy)


   //Here, in order to generate thick arrow  lines, we generate a polygon with
   // a thickness
    var body_path = [0, thickness, len - 5 * thickness, thickness,
      len - 5 * thickness, -1 * thickness, 0, -1 * thickness]
      var head_path = [len - 5 * thickness, 3 * thickness, len, 0,
        len - 5 * thickness, -3 * thickness]

        var angle = Snap.angle(ex, ey, sx, sy)
        var myMatrix = new Snap.Matrix();
        myMatrix.translate(sx , sy);
        myMatrix.rotate(angle, 0, 0);

        if(style == "straight"){
          var line = s.polygon(body_path).transform(myMatrix.toTransformString())
          line.attr({
            fill : Utils.hex(color[0], color[1], color[2],color[3]),
            stroke: Utils.hex(color[0], color[1], color[2],color[3]),
          })
        } else if(style == "curved"){
          //This hardcoded values help to locate the tail and the head
          // of the arrow in a nice way, might be changed in the future
          myMatrix.rotate(properties.rotation, 0, 0);

          var line = s.path(describeArc(len/2-2.4*thickness,0,len/2,-90,90,true))
          .transform(myMatrix.toTransformString())
          line.attr({
            "fill-opacity" : 0,
            stroke: Utils.hex(color[0], color[1], color[2]),
          })
        } else if(style == "dashed"){
          var line = s.polyline(sx,sy,ex,ey)
          line.attr({
            "fill-opacity" : 0,
            stroke: Utils.hex(color[0], color[1], color[2]),
            strokeDasharray: "7, 5"
          })
        }

        var head = s.polyline(head_path).transform(myMatrix.toTransformString())
        head.attr({
          "fill-opacity" : 1,
          fill : Utils.hex(color[0], color[1], color[2]),
          stroke: Utils.hex(color[0], color[1], color[2]),
        })
        var g = s.g(line, head)
        return g

      } // end of _renderArrow


      /**
      * Renders a brace
      * @param       {Snap} s  Snap.svg global object
      * @param       {JSON} obj JSON object from Haskell server
      */
      function _renderBrace(s, properties) {
        var [sx, sy] = Utils.scr([properties.startX, properties.startY])
        var [ex, ey] = Utils.scr([properties.endX, properties.endY])
        var len      = Snap.len(ex, ey, sx, sy)
        var color = properties.color
        var angle = Snap.angle(sx, sy, ex, ey)
        var strokeW = properties.thickness

        //Matriceds for rotation of the brace sides
        var mSide1 = new Snap.Matrix();
                      mSide1.translate(0 , 0);
                      mSide1.rotate(angle, sx, sy);

        var mSide2 = new Snap.Matrix();
            mSide2.translate(0 , 0);
            mSide2.rotate(angle, ex, ey);

        var body = s.polyline(sx,sy,ex,ey)
        body.attr({
           "fill-opacity" : 1,
            stroke: Utils.hex(color[0], color[1], color[2]),
            strokeWidth: strokeW
        })

        var side1 = s.polyline(sx, sy + 5,sx , sy - 5).transform(mSide1.toTransformString())
        side1.attr({
           "fill-opacity" : 1,
            stroke: Utils.hex(color[0], color[1], color[2]),
            strokeWidth: strokeW
        })

        var side2 = s.polyline(ex, ey + 5,ex , ey - 5).transform(mSide2.toTransformString())
        side2.attr({
           "fill-opacity" : 1,
            stroke: Utils.hex(color[0], color[1], color[2]),
            strokeWidth: strokeW
        })
        var sides = s.g(side1,side2)
        return s.g(body, sides);

      } // end of _renderBrace

      /**
      * Renders a rectangle on the canvas
      * @param       {Snap} s  Snap.svg global object
      * @param       {JSON} properties JSON object from Haskell server
      */
      function _renderRectangle(s, properties) {
        [x, y] = [properties.x, properties.y]
        // TODO: document the different btw coord systems
        var rect = s.rect(x - properties.sizeX/2, y - properties.sizeY/2, properties.sizeX, properties.sizeY);
        var color = properties.color;
        rect.attr({
          fill: Utils.hex(color[0], color[1], color[2]),
          "fill-opacity": color[3],
        });
        return rect
      }

      /**
      * Renders a line segment on the canvas
      * @param       {Snap} s  Snap.svg global object
      * @param       {JSON} properties JSON object from Haskell server
      */
      function _renderLine(s, properties) {
        var path = [[properties.startX, properties.startY], [properties.endX, properties.endY]];
        // var path = properties.path;
        // console.log(path);
        var curve = s.path(Utils.path_str(path));
        curve.data("name", properties.name)
        var color = properties.color;
        // by default, the curve should be solid
        curve.attr({
          // fill: "transparent",
          strokeWidth: properties.thickness,
          stroke: Utils.hex(color[0], color[1], color[2]),
          "stroke-opacity": color[3],
        });

        line.attr({
          "fill-opacity" : 0,
          stroke: Utils.hex(color[0], color[1], color[2]),
          strokeDasharray: "7, 5"
        })

        if(style == "dashed") {
          line.attr({ strokeDasharray: "7, 5" });
        }
        return line;
      }

      /**
      * Renders a Right Angle Mark (PerpMark) on the canvas
      * @param       {Snap} s  Snap.svg global object
      * @param       {JSON} properties JSON object from Haskell server
      */
      function _renderRightArc(s,properties){
        var size = properties.size
        var color = properties.color
        var x = properties.x
        var y = properties.y
        var style = properties.style
        var rotation = properties.rotation

        var myMatrix = new Snap.Matrix();
        myMatrix.translate(0, 0);
        myMatrix.rotate(rotation, x, y);
        if (style == "wedge") {
          var f = s.rect(x,y - size ,size,size).transform(myMatrix.toTransformString());
          f.attr({
            fill: Utils.hex(0, 0, 205),
            "fill-opacity": 0.6,
          });
          return f;
        } else{
          var perpMarkPath = [x,y-size,x+size,y-size,x+size,y]
          var p = s.polyline(perpMarkPath).transform(myMatrix.toTransformString())
          p.attr({
            fill: Utils.hex(color[0], color[1], color[2]),
            "fill-opacity": 0,
            stroke: Utils.hex(color[0], color[1], color[2]),
            strokeWidth: 2
          });
          return p;
        }
      }

      /**
      * Renders a Regular Angle Mark on the canvas
      * @param       {Snap} s  Snap.svg global object
      * @param       {JSON} properties JSON object from Haskell server
      */
      function _renderRegularArc(s,properties){
          var radius = properties.r
          var color = properties.color
          var x = properties.x
          var y = properties.y
          var style = properties.style
          var angle = properties.angle > 360.0 ? 360.0
                      - properties.angle : properties.angle //Handle angle > 360
              angle = angle < 0 ? 360 + angle : angle //Handle angle < 0
          var rotation = properties.rotation + 90 - angle

          var startAngle = rotation
          var endAngle = angle < 0 ? (rotation - angle) : (rotation + angle)

          var arcPath = describeArc(x, y, radius, startAngle,endAngle,false);
          var arc = s.path(arcPath);
          arc.attr({
              "fill-opacity": 0,
              stroke: Utils.hex(color[0], color[1], color[2]),
              strokeWidth: 2
          });

          if (style == "wedge") {
              var pf = describeFullArc(x, y, radius, startAngle, endAngle);
              var f = s.path(pf);
              f.attr({
                  // TODO: hardcoded color value here
                  fill: Utils.hex(0, 0, 205),
                  "fill-opacity": 0.6,
              });
              var g = s.g(arc,f)
          } else{
              return arc;
          }
      }

          /**
          * Renders an angle mark on the canvas
          * @param       {Snap} s  Snap.svg global object
          * @param       {JSON} properties JSON object from Haskell server
          */
          function _renderArc(s, properties) {
            if(properties.isRight == "true") {
              return _renderRightArc(s,properties)
            } else {
              return _renderRegularArc(s,properties)
            }
          }

          /**
          * Renders a parallelogram on the canvas
          * @param       {Snap} s  Snap.svg global object
          * @param       {JSON} obj JSON object from Haskell server
          */
          function _renderParallelogram(s, properties) {
            [x, y] = [properties.x, properties.y] // The bottom left corner of the parallelogram
            var sizeX = properties.lengthX;
            var sizeY = properties.lengthY;
            var angle = properties.angle;
            var rotation = properties.rotation;

            var myMatrix = new Snap.Matrix();
            myMatrix.translate(0,0);
            myMatrix.rotate(rotation, x, y);

            h = sizeY * getSinFromDegrees(angle)  // The height of the prallelogram
            cor = sizeY * getCosFromDegrees(angle) // The "shift" we get from the angle, or the
                                                   // "correction" between rectangle to parallelogram

            parallelogramPath = [x, y, x + sizeX , y , x + sizeX + cor, y - h ,
                x + cor , y - h]

              var parallelogram = s.polygon(parallelogramPath)
              .transform(myMatrix.toTransformString())

              var color = properties.color
              var strokeColor = properties["stroke-color"]
              var opacity = color[3]

              parallelogram.attr({
                fill: Utils.hex(color[0], color[1], color[2]),
                "fill-opacity": opacity,
                "stroke": Utils.hex(strokeColor[0], strokeColor[1], strokeColor[2], strokeColor[3])
              });
              if(properties["stroke-style"] == "dashed") {
                parallelogram.attr({ strokeDasharray: "7, 5" })
              }
              return parallelogram;
            }

            // helper method that draws bbox around an object
            function _renderBoundingBox(s, obj) {
              // render the bbox
              var sq = s.path(obj.getBBox().path);
              sq.attr({
                "fill-opacity": 0,
                stroke: "#000",
                strokeWidth: 1
              })
              return sq
            }

            // helper method that draw's bounding circle around an object
            // FIXME: figure out where the center of the circle is
            function _renderBoundingCircle(s, properties) {
              // render Bounding circle
              var bbox = properties.getBBox()
              var circ = s.circle(bbox.cx, bbox.cy, bbox.r0)
              circ.attr({
                "fill-opacity": 0,
                stroke: "#000",
                strokeWidth: 1
              })
            }

            /**
            * TODO [_collectLabels description]
            * @param       {[type]} data [description]
            * @constructor
            * @return      {[type]}      [description]
            */
            async function _collectLabels(data) {
              var res = {}
              var promises = []
              for (var key in data) {
                var record = data[key]
                var properties = record[1]
                if(record[0] == 'Text') {
                  var text = properties.string.contents
                  if(text != "") {
                    var label = await tex2svg("$" + text + "$", properties.name.contents)
                    // var parser = new DOMParser();
                    // var doc = parser.parseFromString(svg, "image/svg+xml");
                    // document.getElementsByTagName('body')[0].appendChild(doc.documentElement);
                    res[label.name] = label.svg
                  }
                }
              }
              return res
            }


            return {
              scene: _renderScene,
              collectLabels: _collectLabels
            };
          })(); // end of Render namespace
