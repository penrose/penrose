import { FnDef } from "./NumFuzz";

/**
 * signedDistance: (
 *   _context: Context,
 *   [t, s]: [string, any],
 *   p: ad.Num[]
 */
export const fnSignedDistanceCircle: FnDef = {
  fnName: "signedDistance",
  shapeType: "Circle",
  args: [
    {
      tag: "Context",
      name: "_context",
    },
    {
      tag: "VectorV",
      name: "p",
      contents: [0, 0],
    },
    {
      tag: "VectorRand",
      name: "center",
      shapeProp: true,
      contents: [
        {
          tag: "FloatRand",
          name: "x",
          min: -50,
          max: 50,
        },
        {
          tag: "FloatRand",
          name: "y",
          min: -50,
          max: 50,
        },
      ],
    },
    {
      tag: "FloatV",
      shapeProp: true,
      name: "r",
      contents: 10,
    },
  ],
};

/**
 * radiusx: ad.Num,
 * radiusy: ad.Num,
 * center: ad.Num[],
 * pInput: ad.Num[]
 */
export const fnSdfEllipseAsNums: FnDef = {
  fnName: "sdEllipseAsNums",
  args: [
    {
      tag: "FloatRand",
      name: "radiusx",
      min: 0.001,
      max: 100,
    },
    {
      tag: "FloatRand",
      name: "radiusy",
      min: 0.001,
      max: 100,
    },
    {
      tag: "FloatRand",
      name: "centerx",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "centery",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "px",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "py",
      min: -50,
      max: 50,
    },
  ],
};

/**
 * _context: Context,
 * p: ad.Num[],
 * q: ad.Num[],
 * r: ad.Num[]
 */
export const fnInradius: FnDef = {
  fnName: "inradius",
  args: [
    {
      tag: "Context",
      name: "_context",
    },
    {
      tag: "FloatRand",
      name: "px",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "py",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "qx",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "qy",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "rx",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "ry",
      min: -50,
      max: 50,
    },
  ],
};
/**
 * signedDistance: (
 *   _context: Context,
 *   [t, s]: [string, any],
 *   p: ad.Num[]
 */
export const fnSignedDistanceEllipse: FnDef = {
  fnName: "signedDistance",
  shapeType: "Ellipse",
  args: [
    {
      tag: "Context",
      name: "_context",
    },
    {
      tag: "FloatRand",
      name: "rx",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "ry",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "centerx",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "centery",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "px",
      min: -50,
      max: 50,
    },
    {
      tag: "FloatRand",
      name: "py",
      min: -50,
      max: 50,
    },
  ],
};
