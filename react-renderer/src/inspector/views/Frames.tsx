import * as React from "react";
import { ObjectInspector } from "react-inspector";
import IViewProps from "./IViewProps";
// https://goessner.net/articles/JsonPath/
class Frames extends React.Component<IViewProps> {
  public render() {
    const { instances, selectedInstance, selectedInstanceFrame } = this.props;
    const selected = instances[selectedInstance];
    if (!selected) {
      return <p>empty</p>;
    }
    const frame =
      selectedInstanceFrame === -1
        ? selected[selected.length - 1]
        : selected[selectedInstanceFrame];
    return (
      <div style={{ padding: "1em" }}>
        {selected.length > 0 ? <ObjectInspector data={frame} /> : <p>empty</p>}
      </div>
    );
  }
}

export default Frames;
