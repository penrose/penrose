import { TabNode } from "flexlayout-react";
import { useUpdateNodeToDiagramCreator } from "../state/atoms";
import FileButton from "./FileButton";

export default function NewTab({ node }: { node: TabNode }) {
  const updateNodeToDiagramCreator = useUpdateNodeToDiagramCreator();
  return (
    <div>
      <h1>create</h1>
      <p>todo sub,sty,dsl</p>
      <FileButton
        name="diagram rendering"
        onClick={() => updateNodeToDiagramCreator(node)}
      />
    </div>
  );
}
