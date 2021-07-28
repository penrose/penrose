import * as React from "react";
import IViewProps from "./IViewProps";
import {
  Canvas,
  updateStateVaryingVals,
  addWeightedVecs,
  PenroseState,
} from "@penrose/core";
import { round } from "lodash";

interface IState {
  nullSpaceVecWeights: number[];
}

class Jacobian extends React.Component<IViewProps, IState> {
  public readonly state = {
    nullSpaceVecWeights: this.props.frame.params.nullspaceVectorWeights,
  };

  public setVecWeight = (vecNum: number, weight: number) => {
    const { frame } = this.props;
    if (!frame.params.nullspaceVectors) {
      return;
      this.setState({ nullSpaceVecWeights: [] }); // if i just resampled
    } else {
      const a = frame.params.nullspaceVectorWeights;
      a[vecNum] = weight; // aliases to the state
      this.setState({ nullSpaceVecWeights: [...a] });
      this.modPenroseState();
    }
  };

  public modPenroseState = () => {
    const { frame, modShapes } = this.props;
    const { nullSpaceVecWeights } = this.state;

    const foldWeightedVecIntoVaryingVals = (
      acc: number[],
      weight: number,
      vecIndex: number
    ): number[] => {
      console.log(frame.params.nullspaceVectors);
      if (!frame.params.nullspaceVectors) {
        console.log("Warning: nullspacevecs dont exist");
        return acc;
      }
      const vec = [...frame.params.nullspaceVectors[vecIndex]];
      try {
        const res = addWeightedVecs(acc, 1, vec, weight);
        return res;
      } catch (e) {
        console.log(`\
Warning: could not add ${frame.params.nullspaceVectors[vecIndex]}\
 with weight ${weight} to the varying values.`);
        return acc;
      }
    };

    const newVaryingVals = nullSpaceVecWeights.reduce(
      (acc, weight, vecIndex) =>
        foldWeightedVecIntoVaryingVals(acc, weight, vecIndex),
      [...frame.varyingValues]
    );

    const newFrame = updateStateVaryingVals({ ...frame }, newVaryingVals);

    // modShapes(newFrame);

    // reset state so stepToConvergence will optimize again
    // this is super buggy, nope

    // frame.params.optStatus = 'NewIter'

    modShapes({
      ...newFrame,
      params: {
        ...newFrame.params,
        optStatus: "NewIter",
      },
    });
  };

  public render() {
    const { frame } = this.props;
    if (frame === null) {
      return <div />;
    }

    if (
      !frame.params.nullspaceVectors ||
      frame.params.optStatus === "NewIter"
    ) {
      return (
        <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
          nullspace vectors have not yet been calculated
        </div>
      );
    } else if (frame.params.nullspaceVectors.length === 0) {
      return (
        <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
          no nullspace vectors available to toggle
        </div>
      );
    } else {
      const sliders = (
        <div
          style={{
            display: "flex",
            width: "100%",
            height: "100%",
            overflow: "hidden",
          }}
        >
          <div style={{ padding: "1em" }}>
            {frame.params.nullspaceVectors.map((vec, ind) => {
              return (
                <SliderInput
                  key={ind}
                  min={-500}
                  max={500}
                  value={frame.params.nullspaceVectorWeights[ind]}
                  canvas={frame.canvas}
                  id={`nullspace vector ${ind} coeff`}
                  num={ind}
                  modAttr={this.setVecWeight}
                  frame={frame}
                />
              );
            })}
          </div>
          <div
            style={{
              // BUG: scroll doesnt really work
              padding: "1em 1em 1em 1em",
              overflow: "auto",
              height: "100%",
              flexBasis: 0,
              flexGrow: 1,
              boxSizing: "border-box",
            }}
          ></div>
        </div>
      );

      return sliders;
    }
  }
}

interface Slider {
  min: number;
  max: number;
  value: number;
  canvas: Canvas;
  id: string;
  num: number;
  modAttr(vecNum: number, vecWeight: number): void;
  frame: PenroseState;
}

class SliderInput extends React.Component<Slider> {
  public readonly state = {
    value: this.props.value,
  };

  public updateAttr = (id: string, evalue: number, num: number) => {
    const newstate = {
      value: evalue,
    };
    this.setState(newstate); // will update span values - could be phased out if spans are set manually
    this.props.modAttr(num, newstate.value);
  };

  public handleChange = (
    eattr: string,
    event: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>,
    num: number
  ) => {
    // sometimes this is undefined... why?
    const teval = Number(event.target.value);
    // console.log(teval);
    //  ? event.target.value
    //  : +event.target.value; // convert to number if necessary
    this.updateAttr(eattr, teval, num);
  };

  public makeRange = () => {
    const { min, max, canvas, id, num, frame } = this.props;
    /*
    if (frame.params.optStatus === 'UnconstrainedRunning' || 'NewIter'){
      return (
        <input
          id={id}
          type="range"
          onChange={(e) => this.handleChange(id, e, num)}
          min={min}
          max={max}
          value={0}
        />
      )
    } */
    return (
      <input
        id={id}
        type="range"
        onChange={(e) => this.handleChange(id, e, num)}
        min={min}
        max={max}
        value={round(this.state.value)}
      />
    );
  };

  public getSpan = () => {
    return round(this.state.value);
  };

  public makeSpan = () => {
    // will display color as hex and checkbox as true/false
    return <span style={{ display: "block" }}>{this.getSpan()}</span>;
  };

  public makeLabel = () => {
    return (
      <label htmlFor={this.props.id}>
        {this.makeSpan()}
        {this.props.id}
      </label>
    );
  };

  public makeInputAndLabel = () => {
    return (
      <div style={{ display: "inline-block" }} className="labinput">
        {this.makeRange()}
        {this.makeLabel()}
      </div>
    );
  };

  public render() {
    return this.makeInputAndLabel();
  }
}

export default Jacobian;
