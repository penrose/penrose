import * as React from "react";
import "./App.css";

import AceEditor from "react-ace";
import Renderer from "react-renderer";
import logo from "./logo.svg";

interface IState {
  code: string;
}

class App extends React.Component<any, IState> {
  public state = { code: "" };
  public compile = async () => {
    fetch("http://localhost:3939/", {
      method: "POST",
      body: this.state.code
    });
  };
  public onChangeCode = (value: string) => {
    this.setState({ code: value });
  };
  public render() {
    const { code } = this.state;
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to Penrose</h1>
        </header>
        <AceEditor onChange={this.onChangeCode} value={code} />
        {/* <textarea onChange={this.onChangeCode} value={code} /> */}
        <button onClick={this.compile}>COMPILE!</button>
        <Renderer />
      </div>
    );
  }
}

export default App;
