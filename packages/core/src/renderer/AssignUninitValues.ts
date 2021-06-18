import { Shape } from "types/shape";
import { State } from "types/state";
import { Color } from "types/value";
import { hsvToRGB } from "utils/Util";
import { RGB } from "./Color";
import { random_palette } from "./ColorData";
import {
  assignNewColors,
  includeShapesOnly,
  shapeListToDistanceGraph,
} from "./ColorProcesses";
import { distGraphToKNNGraph, KNNGraphToColorList } from "./KNNColorProcesses";

const getUninitializedColorChecker = (
  state: State
): ((shape: Shape) => boolean) => {
  const uninitPathsList = state.uninitializedPaths;

  const isColorPath = (path: any): boolean => {
    return path.tag === "PropertyPath" && path.property.value === "color";
  };

  const iconColorPathList = uninitPathsList.filter(isColorPath);

  const getPathName = (path: any): string => {
    return path.name.contents.value + "." + path.field.value;
  };

  const pathNameList = iconColorPathList.map(getPathName);

  const hasUninitializedColor = (shape: Shape): boolean => {
    return pathNameList.includes(shape.properties.name.contents);
  };

  return hasUninitializedColor;
};

export const colorUninitShapes = (state: State): State => {
  const hasUninitializedColor = getUninitializedColorChecker(state);

  const isUninitializedColorShape = (shape: Shape): boolean => {
    return hasUninitializedColor(shape) && includeShapesOnly(shape);
  };

  return getNewlyColoredState(
    state,
    isUninitializedColorShape,
    random_palette(),
    0.8
  );
};

const pointInShape = (shape: Shape, point: [number, number]): boolean => {
  const [px, py] = point;
  switch (shape.shapeType) {
    case "Circle":
      var [cx, cy] = shape.properties.center.contents as [number, number];
      var r = shape.properties.r.contents;
      return Math.sqrt((cx - px) * (cx - px) + (cy - py) * (cy - py)) <= r;

    case "Ellipse":
      var [cx, cy] = shape.properties.center.contents as [number, number];
      var [dx, dy] = [px - cx, py - cy];
      var theta = Math.atan2(dy, dx); // this is in radians
      var [rx, ry] = [
        shape.properties.rx.contents,
        shape.properties.ry.contents,
      ] as [number, number]; // width, height of ellipse
      var [xcomp, ycomp] = [dx * Math.cos(theta), dy * Math.sin(theta)];
      return (
        cx - rx <= xcomp &&
        xcomp <= cx + rx &&
        cy - ry <= ycomp &&
        ycomp <= cy + ry
      );

    case "Rectangle":
      var [cx, cy] = shape.properties.center.contents as [number, number];
      var [w, h] = [
        shape.properties.w.contents,
        shape.properties.h.contents,
      ] as [number, number];
      return cx - w <= px && px <= cx + w && cy - h <= py && py <= cy + h;

    case "Callout": // doesn't do precise calculation of the callout anchor
      var [cx, cy] = shape.properties.center.contents as [number, number];
      var [w, h] = [
        shape.properties.w.contents,
        shape.properties.h.contents,
      ] as [number, number];
      return cx - w <= px && px <= cx + w && cy - h <= py && py <= cy + h;

    case "Square":
      var [cx, cy] = shape.properties.center.contents as [number, number];
      var s = shape.properties.side.contents as number;
      return cx - s <= px && px <= cx + s && cy - s <= py && py <= cy + s;

    default:
      throw new Error(
        "pointInShape detection of " + shape.shapeType + " is not yet supported"
      );
    // case "FreeformPolygon":
    // case "Line":
    // case "Arrow":
    // case "Path":
    // case "Image":
    // case "PathString":
    // case "Polyline"
    // case "Polyline":
    // case "Polygon": // what's the diff between freeformPolygon and polygon?
    //   may use winding number algorithm here, or raycasting
    //   return false;
  }
};

// to be called after shapes have been assigned
export const colorUninitText = (state: State): State => {
  const hasUninitializedColor = getUninitializedColorChecker(state);

  const isUninitializedColorText = (shape: Shape): boolean => {
    return hasUninitializedColor(shape) && shape.shapeType === "Text";
  };

  const isUninitializedColorShape = (shape: Shape): boolean => {
    return hasUninitializedColor(shape) && includeShapesOnly(shape);
  };

  const textToAssignColors = state.shapes.filter(isUninitializedColorText);
  // this has a .center property i believe

  // make a colorlist for the text .___.

  // const shapeArea = (shape : Shape) : number => {
  // :( doesn't work if two shapes of the same area intersect
  // better yet, figure out which shape is on top of which
  // }

  // use state.shapeOrdering!!

  const uninitializedColorShapes = state.shapes.filter(
    isUninitializedColorShape
  );
  const uninitializedColorShapeNames = uninitializedColorShapes.map((elem) => {
    return elem.properties.name.contents;
  });

  // shapes at the beginning of the list are drawn first
  const shapeLayeringOrder = state.shapeOrdering.filter((elem) => {
    return uninitializedColorShapeNames.includes(elem);
  });

  // shapes at the beginning of the list are drawn last
  const reversedShapeLayeringOrder = shapeLayeringOrder.reverse();

  // wow, currying!
  const isMatchingShape = (shapeName: string) => {
    return (shape: Shape) => {
      return shapeName === shape.properties.name.contents;
    };
  };

  console.log("Here");
  // console.log(reversedShapeLayeringOrder);
  const shapeNameAndShapePairs: [
    string,
    Shape
  ][] = reversedShapeLayeringOrder.map((shapeName) => {
    const matchesName = isMatchingShape(shapeName);
    const shape = uninitializedColorShapes.find(matchesName);
    if (typeof shape === "undefined") {
      throw new Error("im sad");
    }
    return [shapeName, shape];
  });
  console.log(shapeNameAndShapePairs);

  // fuck, i need to figure out which one of these are shapes

  var colorList: RGB[] = [];
  for (var i = 0; i < textToAssignColors.length; i++) {
    var textObj = textToAssignColors[i];
    // check text.properties.center

    var textCenter = textObj.properties.center.contents as [number, number];

    var topmostShapeThatPointIsIn;
    for (var j = 0; j < shapeNameAndShapePairs.length; j++) {
      const [shapeName, shape] = shapeNameAndShapePairs[j];
      if (pointInShape(shape, textCenter)) {
        topmostShapeThatPointIsIn = shape;
        break;
      }
    }

    var color = undefined;
    if (typeof topmostShapeThatPointIsIn === "undefined") {
      color = [0, 0, 0]; // black, for visibility against a white canvas
    } else {
      // check the color that the shape was assigned
      const shapeColorObj = topmostShapeThatPointIsIn.properties.color
        .contents as Color<number>;
      var shapeColor = shapeColorObj.contents;
      var shapeColorType = shapeColorObj.tag;

      if (shapeColorType === "RGBA") {
        const [r, g, b, a] = [
          shapeColor[0],
          shapeColor[1],
          shapeColor[2],
          shapeColor[3],
        ];

        // https://math.stackexchange.com/questions/556341/rgb-to-hsv-color-conversion-algorithm
        const v = Math.max(r, g, b);

        // the h and s values are not used, placeholder for now
        shapeColor = [-1, -1, v, a];
        shapeColorType = "HSVA";
        console.log("rgba first");
      }

      if (shapeColorType === "HSVA") {
        // check the v value
        console.log("hsva next");
        const [v, a] = [shapeColor[2], shapeColor[3]];
        console.log(v);

        // the following alpha and value thresholds are arbitrary

        if (a <= 0.35) {
          color = [0, 0, 0]; // black, if it's mostly transparent
        } else if (v < 0.65) {
          // alpha <= 0.6
          color = [1, 1, 1]; // white
        } else {
          // alpha > 0.6 and value >= 0.5
          color = [0, 0, 0]; // black
        }
      }
    }

    if (typeof color === "undefined") {
      throw new Error("Color not assigned");
    }

    colorList.push(color as RGB);
  }
  /* making the colorlist
  loop over the list of (uninit color) text objects
  	find the (smallest) shape that it is contained in 
	get the rgba color of that shape
	use the rgba color to determine the text color (white or black)
	push the rgba color of the text into the colorList
   */

  // now make the colorlist, for appropriate text (need to check if
  // the shape the text is contained in has a dark or light hue)

  return assignNewColors(state, colorList, isUninitializedColorText, 1);
};

const getNewlyColoredState = (
  state: State,
  includeInColorAdjustmentFn: (s: Shape) => boolean,
  palette = random_palette(),
  alpha = 0.8
): State => {
  const shapesToAssignColors = state.shapes.filter(includeInColorAdjustmentFn);
  const distanceGraph = shapeListToDistanceGraph(shapesToAssignColors);
  if (distanceGraph.length <= 3) {
    var k = distanceGraph.length - 1; // number of neighbors
  } else {
    var k = 3;
  }
  if (k <= 0) {
    // figure something out here, refactor the previous functions
    return state;
  } else {
    const KNNGraph = distGraphToKNNGraph(distanceGraph, k);
    const colorList = KNNGraphToColorList(KNNGraph, k, palette);
    const newState = assignNewColors(
      state,
      colorList,
      includeInColorAdjustmentFn,
      alpha
    );
    return newState;
  }
};

// on the new state, assign text colors?
