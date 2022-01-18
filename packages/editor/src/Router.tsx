import {
  BrowserRouter as Router,
  Route,
  useLocation,
  Navigate,
  Routes,
} from "react-router-dom";
import App from "./App";

const Redirector = () => {
  const params = useLocation().search;
  return (
    <Navigate
      replace
      to={{
        pathname: "/",
      }}
      state={{ authed: true, params }}
    />
  );
};

const Routing = () => {
  return (
    <Router>
      <Routes>
        <Route path="/authed" element={<Redirector />} />
        <Route path="/repo" element={<App />} />
        <Route path="/gist/:gistId" element={<App />} />
        <Route path="/" element={<App />} />
      </Routes>
    </Router>
  );
};

export default Routing;
