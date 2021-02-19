import * as React from "react";
import { ObjectInspector } from "react-inspector";
import IViewProps from "./IViewProps";
// https://goessner.net/articles/JsonPath/
class Frames extends React.Component<IViewProps> {
  public render() {
    const { history, frame } = this.props;
    if (frame === null) {
      return <p style={{ padding: "1em" }}>empty</p>;
    }
    return (
      <div style={{ padding: "1em" }}>
        {history.length > 0 ? <ObjectInspector data={frame} /> : <p>empty</p>}
      </div>
    );
  }
}

export default Frames;
