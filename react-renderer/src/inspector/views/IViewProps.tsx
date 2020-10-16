export interface IViewProps {
  // Switches the current frame to index in history
  selectFrame(frame: number): void;
  // Gives access to the full history of frames
  history: State[];
  // Quick access to the frame being shown
  frame: State | null;
  // Either a valid index in History
  frameIndex: number;
  modShapes(state: IState): void; // todo - null check
}
export default IViewProps;
