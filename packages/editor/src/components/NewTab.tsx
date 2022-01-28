import { TabNode } from "flexlayout-react";
import { IWorkspace } from "../types/FileSystem";
import FileButton from "./FileButton";

export default function NewTab({
  node,
  updateNodeToDiagramCreator,
}: {
  node: TabNode;
  updateNodeToDiagramCreator(node: TabNode): void;
}) {
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
