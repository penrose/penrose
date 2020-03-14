import * as React from "react";
import { ObjectInspector } from "react-inspector";
import IViewProps from "./IViewProps";
// https://goessner.net/articles/JsonPath/
class Frames extends React.Component<IViewProps> {
  public render() {
    const { instances, selectedInstance, selectedInstanceFrame } = this.props;
    const selected = instances[selectedInstance];
    return (
      <div style={{ padding: "1em" }}>
        {selected && selected.length > 0 ? (
          <ObjectInspector
            data={
              selectedInstanceFrame === -1
                ? selected[selected.length - 1]
                : selected[selectedInstanceFrame]
            }
          />
        ) : (
          <p>empty</p>
        )}
      </div>
    );
  }
}

export default Frames;
