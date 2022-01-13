import { constOf } from "engine/Autodiff";
import { FloatV, makeCanvas, sampleBlack, VectorV } from "shapes/Samplers";
import { makeRectangle } from "shapes/Rectangle";
import { makeCircle } from "shapes/Circle";

const canvas = makeCanvas(800, 700);

//                                                                                                      
//                                         ▲                                                            
//                                       y ║400                                                         
//                       ┌─────────────────╬─────────────────┐                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║300              │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │                 ║                 │                                          
//                       │ _rectangles[3]  ║200              │                                          
//     ┌─────────────────┴─────────────────╬─────────────────┴─────────────────┐                        
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │                                   ║100                                │                        
//     │                 ┌─────────────────╬─────────────────┬─────────────────┼─────────────────┐      
//     │                 │                 ║                 │                 │                 │      
//     │                 │                 ║                 │                 │                 │      
//     │                 │                 ║                 │                 │                 │      
//     │                 │                 ║                 │                 │                 │      
//     │                 │                 ║                 │                 │                 │      
//     │-200             │-100             ║0                │100              │200              │300   
//  ═══╬═════════════════╬═════════════════╬═════════════════╬═════════════════╬═════════════════╬═══▶  
//     │                 │                 ║                 │                 │                 │ x    
//     │                 │                 ║                 │                 │                 │      
//     │                 │                 ║                 │                 │                 │      
//     │                 │                 ║                 │                 │                 │      
//     │                 │                 ║                 │                 │                 │      
//     │                 │ _rectangles[1]  ║-100             │ _rectangles[2]  │                 │      
//     │                 └─────────────────╬─────────────────┴─────────────────┼─────────────────┘      
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │                                   ║                                   │                        
//     │ _rectangles[0]                    ║-200                               │                        
//     └───────────────────────────────────╬───────────────────────────────────┘                        
//                                         ║                                                            
//                                                                                                      
// Created with Monodraw                                                                                

export const _rectangles = [ 
  // _rectangles[0]
  makeRectangle(canvas, {
    center: VectorV([0, 0].map(constOf)),
    width: FloatV(constOf(400)),
    height: FloatV(constOf(400)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // _rectangles[1]
  makeRectangle(canvas, {
    center: VectorV([0, 0].map(constOf)),
    width: FloatV(constOf(200)),
    height: FloatV(constOf(200)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // _rectangles[2]
  makeRectangle(canvas, {
    center: VectorV([200, 0].map(constOf)),
    width: FloatV(constOf(200)),
    height: FloatV(constOf(200)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // _rectangles[3]
  makeRectangle(canvas, {
    center: VectorV([0, 300].map(constOf)),
    width: FloatV(constOf(200)),
    height: FloatV(constOf(200)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  })
];

export const _circles = [
  // _circles[0]
  makeCircle(canvas, {
    r: FloatV(constOf(200)),
    center: VectorV([0, 0].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // _circles[1]
  makeCircle(canvas, {
    r: FloatV(constOf(100)),
    center: VectorV([0, 0].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // _circles[2]
  makeCircle(canvas, {
    r: FloatV(constOf(100)),
    center: VectorV([200, 0].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // _circles[3]
  makeCircle(canvas, {
    r: FloatV(constOf(100)),
    center: VectorV([0, 300].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
];

export const _lines = [
  // TODO: _lines[0]
  // TODO: _lines[1]
  // TODO: _lines[2]
  // TODO: _lines[3]
];

export const _polygons = [
  // TODO: _polygons[0]
  // TODO: _polygons[1]
  // TODO: _polygons[2]
  // TODO: _polygons[3]
];
