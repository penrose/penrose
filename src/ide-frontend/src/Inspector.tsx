import * as React from "react";

import {Popout} from "react-popout-component";

interface IProps {
  show: boolean;
}

class Inspector extends React.Component<IProps> {
  public render() {
    const {show} = this.props;
    return (
      <div style={{display: "none"}}>
        {show && <Popout>
            <h1>Inspector</h1>
        </Popout>}
      </div>);
  }
}

export default Inspector;