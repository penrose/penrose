import * as React from "react";
import IViewProps from "./IViewProps";
import { colorUninitShapes, 
  colorUninitText,
	validColorMapNames,
	PenroseState } from "@penrose/core";

interface IState {
	originalState: PenroseState | undefined
}

class Palette extends React.Component<IViewProps, IState> {
	public readonly state = { 
		originalState: undefined,
	}

	public updateShapeColors = (state : PenroseState, paletteName : string) : PenroseState => {
		return colorUninitText(colorUninitShapes(state, paletteName))
	}

	public modAttr = (paletteName: string) => {
    const { frame, modShapes, frameIndex, history } = this.props;
    if (frameIndex === -1 || frameIndex === history.length - 1) {
			if (!this.state.originalState){
				this.setState({
					...this.state,
					originalState: frame
				})
			}
			if (paletteName === 'None'){
				modShapes(this.state.originalState) // this doesn't work the way i want it to :(
			} else {
				const newFrame = this.updateShapeColors(frame, paletteName) as PenroseState;
				modShapes({...newFrame});
			}
    } 
  };

	public render(){
		
		if (!this.props.frame){ 
			return (<div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
				no frame
			</div>
			)
		}

		else if (this.props.frame.params.optStatus !== 'EPConverged'){
			return (<div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
			state not converged
		</div>
		)
		}
		
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>

      <select id={'palettePicker'} onChange = {(e) => {this.modAttr(e.target.value)}}>
				<option key = {"none"} value = {"None"}>
				</option>
        {validColorMapNames.map((option: string) => (
          <option key={option} value={option}>
            {option}
          </option>
        ))}{" "}
      </select>

      </div>
    );
	}
}

export default Palette;