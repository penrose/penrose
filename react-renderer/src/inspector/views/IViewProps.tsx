export interface IViewProps {
  selectFrame(frame: number): void;
  history: State[];
  frame: State | null;
  frameIndex: number;
}
export default IViewProps;
