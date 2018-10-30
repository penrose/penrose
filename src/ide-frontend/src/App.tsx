import * as React from "react";
import "./App.css";

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
  public onChangeCode = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    this.setState({ code: e.target.value });
  };
  public render() {
    const { code } = this.state;
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to Penrose</h1>
        </header>
        <textarea onChange={this.onChangeCode} value={code} />
        <button onClick={this.compile}>COMPILE!</button>
      </div>
    );
  }
}

export default App;
