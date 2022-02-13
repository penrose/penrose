import * as React from "react";
import { ObjectInspector } from "react-inspector";
import IViewProps from "./IViewProps";
// https://goessner.net/articles/JsonPath/
class Frames extends React.Component<IViewProps> {
  public render(): JSX.Element {
    const { /*history,*/ frame } = this.props;
    if (frame === undefined) {
      return <p style={{ padding: "1em" }}>empty</p>;
    }
    return (
      <div style={{ padding: "1em" }}>
        {frame ? <ObjectInspector data={frame} /> : <p>empty</p>}
      </div>
    );
  }
}

export default Frames;
