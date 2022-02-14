import { TabNode } from "flexlayout-react";
import { useState } from "react";
import DiagramInitializer from "./DiagramInitializer";
import FileButton from "./FileButton";

export default function NewTab({ node }: { node: TabNode }) {
  const [currentView, setCurrentView] = useState<"all" | "diagram">("all");
  if (currentView === "diagram") {
    return <DiagramInitializer node={node} />;
  }
  return (
    <div>
      <h1>create</h1>
      {/* TODO: new trio altogether - construct based */}
      <p>todo sub,sty,dsl</p>
      <FileButton
        name="diagram rendering"
        onClick={() => setCurrentView("diagram")}
      />
    </div>
  );
}
