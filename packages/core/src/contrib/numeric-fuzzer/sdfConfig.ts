export type fnDef = {
  fnName: string;
  args: fnArg[];
};

export type fnArg = fnVector | fnNum | fnVectorRand | fnNumRand;

export type fnVector = {
  //Point
  tag: "VectorV";
  name: string;
  contents: [fnNum, fnNum];
};

export type fnVectorRand = {
  //Point
  tag: "VectorRand";
  name: string;
  contents: [fnNumRand, fnNumRand];
};

export type fnNum = {
  tag: "FloatV";
  name: string;
  contents: number;
};

export type fnNumRand = {
  //Numeric value
  tag: "FloatRand";
  name: string;
  min: number;
  max: number;
};

export const sampleCircle: fnDef = {
  fnName: "signedDistance",
  args: [
    {
      tag: "VectorV",
      name: "p",
      contents: [
        {
          tag: "FloatV",
          name: "x",
          contents: 0,
        },
        {
          tag: "FloatV",
          name: "y",
          contents: 0,
        },
      ],
    },
    {
      tag: "VectorRand",
      name: "center",
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
      name: "r",
      contents: 10,
    },
  ],
};
