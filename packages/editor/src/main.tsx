import React, { Component, Suspense } from "react";
import ReactDOM from "react-dom";
import "./index.css";
import "flexlayout-react/style/light.css";
import "react-toastify/dist/ReactToastify.css";
import Router from "./Router";
import { RecoilRoot } from "recoil";
import { toast } from "react-toastify";

class ErrorBoundary extends Component {
  constructor(props: any) {
    super(props);
  }
  componentDidCatch(error: any, errorInfo: any) {
    console.error(error, errorInfo);
    toast.error(error.toString());
  }
  render() {
    return this.props.children;
  }
}

ReactDOM.render(
  <React.StrictMode>
    <RecoilRoot>
      <ErrorBoundary>
        <Suspense fallback={<div>Loading...</div>}>
          <Router />
        </Suspense>
      </ErrorBoundary>
    </RecoilRoot>
  </React.StrictMode>,
  document.getElementById("root")
);
