import { IInstanceMap } from "src/Protocol";

interface IViewProps {
  instances: IInstanceMap;
  selectedInstance: string;
  selectedInstanceFrame: number;
}
export default IViewProps;
