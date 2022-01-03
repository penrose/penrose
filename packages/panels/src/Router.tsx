import * as React from "react";
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Redirect,
  useLocation,
} from "react-router-dom";
import App from "./App";

const Redirector = () => {
  const params = useLocation().search;
  return (
    <Redirect
      from="/authed"
      to={{
        pathname: "/",
        state: { authed: true, params },
      }}
    />
  );
};

const Routing = () => {
  return (
    <Router>
      <Switch>
        <Route path="/authed" component={Redirector} />
        <Route path="/repo" component={App} />
        <Route path="/gist/:gistId" component={App} />
        <Route path="/" component={App} />
      </Switch>
    </Router>
  );
};

export default Routing;
