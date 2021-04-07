import { PenroseError, PenroseState } from "@penrose/core";

export interface IViewProps {
  // Switches the current frame to index in history
  selectFrame(frame: number): void;
  // Gives access to the full history of frames
  history: PenroseState[];
  // Quick access to the frame being shown
  frame: PenroseState | null;
  // Either a valid index in History
  frameIndex: number;
  modShapes(state: PenroseState): void; // todo - null check
  error: PenroseError | null;
}
export default IViewProps;
