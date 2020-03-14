import { IInspectState } from "../Inspector";

export interface IViewMethods {
  selectInstanceFrame(frame: number): void;
}
type IViewProps = IViewMethods & IInspectState;
export default IViewProps;
