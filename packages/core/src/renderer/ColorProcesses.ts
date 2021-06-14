
/*
colorPickerMatrix = stateToColorPickerMatrix(state)

colorList = samplePalette(colorPickerMatrix)

// one last fn
colorList --> modified state
*/

import {State} from "types/state";
import {Shape, Properties} from "types/shape";
import {Value, IVectorV} from "types/value";

import {Graph, RGB, samplePalette} from "./Color";

const isCircle = (shape : Shape) : boolean => {
  return shape.shapeType === 'Circle';
}

const createMatrix = (rows : number, cols : number) : Graph => {
  var matrix : number[][] = []
  for (var i=0; i<cols; i++){
    var row : number[] = []
    for (var j=0; j<rows; j++){
      row.push(0.);
    }
    matrix.push(row);
  }
  return matrix;
}

const dist = (v1 : number[], v2 : number[]):number => {
  if (v1.length != v2.length){
    throw new Error("Vector inputs are not of the same dimension");
  }
  var squaredSum = 0.;
  for (var i=0; i<v1.length; i++){
    squaredSum += ((v1[i]-v2[i]) * (v1[i]-v2[i]));
  }
  return Math.sqrt(squaredSum);
}

const stateToColorPickerMatrix = (state : State) : Graph => {

  // state.shapes : Shape[]

  const shapeList = state.shapes

  // get the circles first, as a test.
  const circleList = shapeList.filter(isCircle);

  // construct a matrix of 0's
  var object_graph = createMatrix(circleList.length, circleList.length);

  // fill in the matrix w/ euclidean distance between objects
  // also keep track of a max dist value
  var max_dist = 0.;
  for (var i=0; i<circleList.length; i++){
    for (var j=i+1; j<circleList.length; j++){
      
      const circle1 = circleList[i];
      const circle2 = circleList[j];

      
      const v1 = circle1.properties.center;
      const v2 = circle2.properties.center;
      
      if ((!Array.isArray(v1.contents)) || (!Array.isArray(v2.contents))
        || (v1.contents.length !== v2.contents.length)
        || (typeof v1.contents[0] !== 'number') 
        || (typeof v2.contents[0] !== 'number')){
          throw new Error('bad center input: not number[]');
        }

      // this is hacky, is there a better way to typecheck
      const centerDist = dist(v1.contents as number[], v2.contents as number[]);

      
      if (centerDist > max_dist) {
        max_dist = centerDist;
      }
      
      // super hacky, uses some guidelines fron Constraints.ts (repel fxns)
      object_graph[i][j] = (1/(Math.sqrt(centerDist)+20.))*10e4;
      object_graph[j][i] = object_graph[i][j];
      // object_graph[i][j] = centerDist;
    }
  }
  
  console.log("sqrted graph")
  console.log(object_graph);
  return object_graph; 
}

// idk, stuff is fucked
const updateColors = (state : State, colorList : [number,number,number][]) : State => {
  // assumes all colors map to the order of objects in state
  var newState = state;
  var j = 0;
  for (var i=0; i<newState.shapes.length; i++){
    if (isCircle(newState.shapes[i])){
      newState.shapes[i].properties.color
      = {
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          contents: [colorList[j][0], colorList[j][1], colorList[j][2], 0.5] 
        }
      }
      j+=1;
    }
  }
  return newState;
}

export const updateCircleColors = (state : State) : State => {
  const graph = stateToColorPickerMatrix(state);
  const colorList = samplePalette(graph, 0); 
  const newState = updateColors(state, colorList);
  return newState;
}
  // map over the shapeList for their properties? 

  // list of diff shapes: 
  /*
  Circle,
	Ellipse,
	Square,
	Rectangle,
	Callout,
	Polygon,
	FreeformPolygon,
	Polyline,
	Text: Label,
	Arrow,
	Path,
	Line,
	Image,
	PathString
  */
  // I'll start with Circle, Square, Rectangle
  
  // Well, to start, what can I use from the properties the graph (matrix)? 
  // oof, this is gonna be computationally expensive...                          

  // each shape: 

  /*
  // types/shape
    interface IShape {
      shapeType: string;
      properties: Properties;
    }

    export type Properties = { [k: string]: Value<number> };

  // types/value
    export type Value<T> =
  | IFloatV<T>
  | IIntV
  | IBoolV<T>
  | IStrV
  | IPtV<T>
  | IPathDataV<T>
  | IPtListV<T>
  | IColorV<T>
  | IPaletteV<T>
  | IFileV<T>
  | IStyleV<T>
  | IListV<T>
  | IVectorV<T>
  | IMatrixV<T>
  | ITupV<T>
  | ILListV<T>
  | IHMatrixV<T>
  | IPolygonV<T>;

  export interface IFloatV<T> {
  tag: "FloatV";
  contents: T;
  }

  export interface IPtV<T> {
  tag: "PtV";
  contents: [T, T];
  }

  export interface IColorV<T> {
  tag: "ColorV";
  contents: Color<T>;
  }
  
  export type Color<T> = IRGBA<T> | IHSVA<T>;

	export interface IRGBA<T> {
	tag: "RGBA";
	contents: [T, T, T, T];
	}

	export interface IHSVA<T> {
	tag: "HSVA";
	contents: [T, T, T, T];
	}
  */


