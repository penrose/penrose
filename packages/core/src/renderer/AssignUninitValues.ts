import { Shape } from "types/shape";
import { State } from "types/state";
import { Color } from "types/value";
import { RGB } from "./Color";
import { random_palette } from "./ColorData";
import {
  assignNewColors,
  includeShapesOnly,
  shapeListToDistanceGraph,
} from "./ColorProcesses";
import { distGraphToKNNGraph, KNNGraphToColorList } from "./KNNColorProcesses";

// given a state, returns a (shape => bool) function f:
// f(s) === true if shape s has an uninitialized color path
// f(s) === false if shape s has a (user) initialized color path
const getUninitializedColorCheckerFn = (
  state: State
): ((shape: Shape) => boolean) => {
  // the list of uninitialized paths from the state
  const uninitPathsList = state.uninitializedPaths;

  // checks if a path is a color path
  const isColorPath = (path: any): boolean => {
    return path.tag === "PropertyPath" && path.property.value === "color";
  };

  // gets the list of uninitialized color paths
  const uninitColorPathList = uninitPathsList.filter(isColorPath);

  // given a path, returns the name of the shape it belongs to (ex. H.icon)
  const getPathName = (path: any): string => {
    return path.name.contents.value + "." + path.field.value;
  };

  // get the corresponding shape names of the uninitialized color paths
  const pathNameList = uninitColorPathList.map(getPathName);

  // determines if a shape has an uninitialized color path
  const hasUninitializedColor = (shape: Shape): boolean => {
    return pathNameList.includes(shape.properties.name.contents);
  };

  // return the fn
  return hasUninitializedColor;
};

// assign colors to the shapes w/ uninitialized colors
export const colorUninitShapes = (state: State): State => {
  // get the fn that checks if a shape has an uninitialized color
  const hasUninitializedColor = getUninitializedColorCheckerFn(state);

  // make a new (stricter) fn that also checks
  // if a shape obj satisfies includeShapesOnly
  const isUninitializedColorShape = (shape: Shape): boolean => {
    return hasUninitializedColor(shape) && includeShapesOnly(shape);
  };

  return getNewlyColoredState(
    state,
    isUninitializedColorShape,
    random_palette(),
    0.5
  );
};

const getNewlyColoredState = (
  state: State,
  includeInColorAdjustmentFn: (s: Shape) => boolean,
  palette = random_palette(),
  alpha = 0.5
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
    // this happens if distanceGraph.length <= 1, in which case
    // we don't need to do special coloring (a single object is uncolored)
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

// detects if an (x,y) point is contained within a (limited) number of shapes
// used to determine whether (uninitialized color) text should be black or white
// pointInShape(s, p) === true if p is contained in s, and false otherwise
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

    // note: doesn't do precise calculation of the callout anchor,
    // only the main rectangle box
    case "Callout":
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
    /* 
      unsupported shapes, currently ignored by fns that call this function 
       (and also excluded by includeShapesOnly): 

       - Line, Arrow, Path, Image, PathString, Polyline
       - FreeformPolygon, Polygon

       what's the difference between FreeformPolygon & Polygon? 
       note: may be able to use winding number algorithm, or rays for 
       point detection inside polygons
      */
  }
};

// to be called after shape colors have all been assigned
export const colorUninitText = (state: State): State => {
  // fn that checks if a shape has an uninitialized color path
  const hasUninitializedColor = getUninitializedColorCheckerFn(state);

  // fn that checks if a shape is a text & has an uninit color path
  const isUninitializedColorText = (shape: Shape): boolean => {
    return hasUninitializedColor(shape) && shape.shapeType === "Text";
  };

  // get the text shapes that we need to assign colors to
  const textToAssignColors = state.shapes.filter(isUninitializedColorText);

  // get all the shapes
  const colorShapes = state.shapes.filter(includeShapesOnly);

  const colorShapeNames = colorShapes.map((elem) => {
    return elem.properties.name.contents;
  });

  // shapes at the beginning of the list are drawn first
  const shapeLayeringOrderAll = state.shapeOrdering.filter((elem) => {
    return colorShapeNames.includes(elem);
  });

  // get the shapes that are drawn most recently at the front of the list
  const reversedShapeLayeringOrderAll = shapeLayeringOrderAll.reverse();

  // takes in a shapeName, and returns a fn that checks of a shape has that name
  const isMatchingShape = (shapeName: string) => {
    const fn = (shape: Shape) => {
      return shapeName === shape.properties.name.contents;
    };
    return fn;
  };

  // maps the list of shape names to a list of
  // [shapeName, shape] : [string, Shape] objects
  const shapeNameAndShapePairsAll: [
    string,
    Shape
  ][] = reversedShapeLayeringOrderAll.map((shapeName) => {
    const matchesName = isMatchingShape(shapeName);
    const shape = colorShapes.find(matchesName);
    if (typeof shape === "undefined") {
      throw new Error("im sad");
    }
    return [shapeName, shape];
  });

  // get the colorlist for the text objects
  const colorList = createTextColorList(
    textToAssignColors,
    shapeNameAndShapePairsAll
  );

  // assign the colors to the text objects in a new state
  return assignNewColors(state, colorList, isUninitializedColorText, 1);
};

const createTextColorList = (
  textPathList: any[],
  shapeNameAndShapePairList: [string, Shape][]
): RGB[] => {
  // colorlist to fill up w/ text colors (black or white)
  var colorList: RGB[] = [];

  // push a color into the colorlist, corresponding to each text path object
  for (var i = 0; i < textPathList.length; i++) {
    var textObj = textPathList[i];

    var textCenter = textObj.properties.center.contents as [number, number];

    // the shape that the text is immediately on top of
    var topmostShapeThatPointIsIn;

    // find a shape, if any, that the text center is contained in
    for (var j = 0; j < shapeNameAndShapePairList.length; j++) {
      const [shapeName, shape] = shapeNameAndShapePairList[j];
      if (pointInShape(shape, textCenter)) {
        topmostShapeThatPointIsIn = shape;
        break;
      }
    }

    // initialize a color variable
    var color = undefined;

    // the text center was not found to be inside any shape, so it will be
    // drawn against a white canvas
    if (typeof topmostShapeThatPointIsIn === "undefined") {
      color = [0, 0, 0]; // black, for visibility against a white canvas
    }
    // the text center WAS found to be inside of a shape
    else {
      // check the color that the shape was assigned
      const shapeColorObj = topmostShapeThatPointIsIn.properties.color
        .contents as Color<number>;
      var shapeColor = shapeColorObj.contents;
      var shapeColorType = shapeColorObj.tag;

      if (shapeColorType === "RGBA") {
        // "convert" to HSV to get the V value
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
      }

      // assign color based on the "V" value
      if (shapeColorType === "HSVA") {
        // check the v value
        const [v, a] = [shapeColor[2], shapeColor[3]];

        // the following alpha and value thresholds are arbitrary
        if (a <= 0.35) {
          color = [0, 0, 0]; // black, if it's mostly transparent
        } else if (v < 0.7) {
          // alpha <= 0.6
          color = [1, 1, 1]; // white
        } else {
          // alpha > 0.6 and value >= 0.5
          color = [0, 0, 0]; // black
        }
      }
    }

    // a check to make typescript happy
    // also, Color<number> objects only have "RGBA" or "HSVA" as tags,
    // so this case should never be reached
    if (typeof color === "undefined") {
      throw new Error("Color not assigned");
    }

    colorList.push(color as RGB);
  }
  return colorList;
};
