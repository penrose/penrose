import { createHashRouter, RouterProvider } from "react-router-dom";
import { Content } from "./components/Content";
import All from "./problems/all";
import Chemistry from "./problems/chemistry";
import Geometry from "./problems/geometry";
import Graphs from "./problems/graphs";

const router = createHashRouter([
  {
    path: "/",
    element: <Content />,
  },
  {
    path: "chemistry",
    element: <Chemistry />,
  },
  {
    path: "geometry",
    element: <Geometry />,
  },
  {
    path: "graphs",
    element: <Graphs />,
  },
  {
    path: "problems",
    element: <All />,
  },
]);

function App() {
  return <RouterProvider router={router} />;
}

export default App;
