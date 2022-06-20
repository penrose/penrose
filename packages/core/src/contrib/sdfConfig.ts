export type fnDef = {
  fnName: string;
  args: fnArgDef[];
};

export type fnArgDef = fnShapeDef | fnPointDef;

export type fnPointDef = {
  //Point
  tag: "VectorV";
  name: string;
  contents: [fnNumDef, fnNumDef];
};

export type fnShapeDef = {
  //Shape
  tag: "Shape";
  name: string;
  contents: fnShapePropsDef[];
};

export type fnShapePropsDef = fnPointDef | fnNumDef; //What shape properties can look like

export type fnNumDef = {
  //Numeric value
  tag: "FloatV";
  name: string;
  min: number;
  max: number;
};

export const sampleCircle: fnDef = {
  fnName: "signedDistance",
  args: [
    // {
    //   tag: "VectorV",
    //   name: "p",
    //   contents: [
    //     {
    //       tag: "FloatV",
    //       name: "x",
    //       min: 0,
    //       max: 50, //size of canvas or whatever
    //     },
    //     {
    //       tag: "FloatV",
    //       name: "y",
    //       min: 0,
    //       max: 50,
    //     },
    //   ],
    // },
    {
      tag: "Shape", //[t,s]
      name: "Circle",
      contents: [
        {
          tag: "VectorV",
          name: "center",
          contents: [
            {
              tag: "FloatV",
              name: "x",
              min: -50,
              max: 50,
            },
            {
              tag: "FloatV",
              name: "y",
              min: -50,
              max: 50,
            },
          ],
        },
        {
          tag: "FloatV",
          name: "r",
          min: 0,
          max: 50,
        },
      ],
    },
  ],
};
