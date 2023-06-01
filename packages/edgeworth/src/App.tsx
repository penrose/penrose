import { createHashRouter, RouterProvider } from "react-router-dom";
import { Content } from "./components/Content.js";
import All from "./problems/all.js";
import Chemistry from "./problems/chemistry.js";
import Geometry from "./problems/geometry.js";
import Graphs from "./problems/graphs.js";

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
