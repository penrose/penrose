export const optimizerSeries = (data) => {
  var res = [];
  for (let id in data) {
    let ins = data[id];
    const timeTypes = [
      // "overall",
      ["compilation", "compilation"],
      ["labelling", "state preparation"],
      ["optimization", "optimization"],
      ["rendering", "rendering"],
    ];
    const entries = timeTypes.map(([t, label]) => ({
      size: ins.optProblem.constraintCount + ins.optProblem.objectiveCount,
      time: ins.timeTaken[t],
      id: ins.id,
      name: ins.substanceName,
      nonzeroConstraints: ins.nonzeroConstraints,
      type: label,
    }));
    res = res.concat(entries);
  }
  return res;
};
export const compileSeries = (data) => {
  var res = [];
  for (let id in data) {
    let ins = data[id];
    const entries = [
      {
        matches: ins.selectorMatches.reduce((a, b) => a + b, 0),
        time: ins.timeTaken.compilation,
        nonzeroConstraints: ins.nonzeroConstraints,
        name: ins.substanceName,
        // time: ins.timeTaken.overall,
        id: ins.id,
        type: "compilation",
      },
    ];
    res = res.concat(entries);
  }
  return res;
};
export const optSpec = (data) => {
  return {
    width: 700,
    height: 500,
    $schema: "https://vega.github.io/schema/vega-lite/v4.json",
    // title: "# of constraints and objectives vs. Execution time",
    data: { values: data },
    config: {
      axisX: { labelFontSize: 18, titleFontSize: 18 },
      axisY: { labelFontSize: 18, titleFontSize: 18 },
    },
    selection: {
      constr: {
        type: "single",
        fields: ["nonzeroConstraints"],
        bind: {
          input: "select",
          options: [null, true, false],
          name: "Nonzero-constraints: ",
        },
      },
      timeType: {
        type: "multi",
        fields: ["type"],
        bind: "legend",
      },
    },
    mark: { type: "area" },
    transform: [{ filter: { selection: "constr" } }],
    encoding: {
      x: {
        field: "size",
        type: "quantitative",
        bin: { step: 5 },
        legend: { title: "The number of constraints and objectives" },
      },
      y: {
        aggregate: "mean",
        field: "time",
        type: "quantitative",
        stack: true,
      },
      color: {
        field: "type",
        type: "nominal",
        legend: {
          orient: "top",
          titleFontSize: 18,
          labelFontSize: 18,
        },
      },
      opacity: {
        condition: { selection: "timeType", value: 1 },
        value: 0.2,
      },
    },
  };
};
export const optScatterSpec = (data) => {
  return {
    width: 700,
    height: 500,
    $schema: "https://vega.github.io/schema/vega-lite/v4.json",
    // title: "Problem size vs. Optimization time",
    data: { values: data },
    mark: {
      type: "point",
    },
    config: {
      axisX: { labelFontSize: 18, titleFontSize: 18 },
      axisY: { labelFontSize: 18, titleFontSize: 18 },
    },
    selection: {
      constr: {
        type: "single",
        fields: ["nonzeroConstraints"],
        bind: {
          input: "select",
          options: [null, true, false],
          name: "Nonzero-constraints: ",
        },
      },
    },
    encoding: {
      x: {
        field: "size",
        type: "quantitative",
        title: "Number of constraints and objectives",
      },
      y: {
        field: "time",
        type: "quantitative",
        title: "Execution time (ms)",
      },
      color: {
        field: "type",
        type: "nominal",
        title: "Time type",
      },
      shape: {
        field: "type",
        type: "nominal",
        title: "Time type",
        legend: {
          orient: "top",
          titleFontSize: 18,
          labelFontSize: 18,
        },
      },
      opacity: {
        condition: { selection: "constr", value: 1 },
        value: 0.0,
      },
      tooltip: [
        { field: "size", type: "quantitative" },
        { field: "time", type: "quantitative" },
        { field: "name", type: "nominal" },
        { field: "type", type: "nominal" },
        { field: "id", type: "nominal" },
      ],
    },
  };
};
export const exampleBarSpec = (data) => {
  return {
    width: 700,
    height: 500,
    $schema: "https://vega.github.io/schema/vega-lite/v4.json",
    config: {
      axisX: { labelFontSize: 14, titleFontSize: 18, labelAngle: 45 },
      axisY: { labelFontSize: 18, titleFontSize: 18 },
    },
    data: { values: data },
    mark: "bar",
    encoding: {
      x: {
        field: "name",
        type: "nominal",
        axis: { title: "Example Name" },
        sort: "y",
      },
      y: {
        field: "time",
        type: "quantitative",
        title: "Execution time (ms)",
      },
      color: {
        field: "type",
        type: "nominal",
        title: "Time type",
      },
      tooltip: [
        { field: "size", type: "quantitative" },
        { field: "time", type: "quantitative" },
        { field: "name", type: "nominal" },
        { field: "id", type: "nominal" },
      ],
    },
  };
};
export const compileSpec = (data) => {
  return {
    width: 700,
    height: 500,
    $schema: "https://vega.github.io/schema/vega-lite/v4.json",
    // title: "Selector matches vs. Compilation time",
    config: {
      axisX: { labelFontSize: 18, titleFontSize: 18 },
      axisY: { labelFontSize: 18, titleFontSize: 18 },
    },
    data: { values: data },
    layer: [
      {
        selection: {
          constr: {
            type: "single",
            fields: ["nonzeroConstraints"],
            bind: {
              input: "select",
              options: [null, true, false],
              name: "Nonzero-constraints: ",
            },
          },
        },
        mark: {
          type: "point",
        },
        encoding: {
          x: {
            field: "matches",
            type: "quantitative",
            title: "Selector matches",
          },
          y: {
            field: "time",
            type: "quantitative",
            title: "Compilation time (ms)",
          },
          // color: {
          //   field: "nonzeroConstraints",
          //   type: "nominal",
          //   legend: null
          // },
          tooltip: [
            { field: "matches", type: "quantitative" },
            { field: "time", type: "quantitative" },
            { field: "name", type: "nominal" },
            { field: "id", type: "nominal" },
          ],
          opacity: {
            condition: { selection: "constr", value: 0.8 },
            value: 0.0,
          },
        },
      },
      // {
      //   mark: {
      //     type: "line",
      //     color: "firebrick"
      //   },
      //   transform: [
      //     { filter: { selection: "constr" } },
      //     { regression: "time", on: "matches", method: "log" }
      //   ],
      //   encoding: {
      //     x: {
      //       field: "matches",
      //       type: "quantitative",
      //       title: "Selector Matches"
      //     },
      //     y: { field: "time", type: "quantitative", title: "Time (ms)" },
      //     color: { value: "#dd565c" }
      //   }
      // }
    ],
  };
};
