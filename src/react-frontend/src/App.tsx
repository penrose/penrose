import * as React from "react";
import "./App.css";
import Canvas from "./Canvas";

interface IState {
  json: any;
}

class App extends React.Component<any, IState> {
  public readonly state = { json: {} };
  public async componentDidMount() {
    const myRes = await fetch("/raw.json");
    const myJSON = await myRes.json();
    // tslint:disable-next-line:no-console
    // console.log(myJSON);
    this.setState({ json: myJSON });
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
