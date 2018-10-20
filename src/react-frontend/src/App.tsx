import * as React from "react";
import "./App.css";
import Canvas from "./Canvas";
import { clean } from "./Util";

interface IState {
  json: any;
}

class App extends React.Component<any, IState> {
  public readonly state = { json: {} };
  public async componentDidMount() {
    const myRes = await fetch("/raw.json");
    const myJSON = await myRes.json();
    this.setState({ json: clean(myJSON) });
  }
  public render() {
    const { json } = this.state;
    return (
      <div className="App">
        <Canvas data={json} />
      </div>
    );
  }
}

export default App;
