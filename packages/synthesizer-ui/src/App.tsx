import { createHashRouter, RouterProvider } from "react-router-dom";
import { Content } from "./components/Content";
import Chemistry from "./problems/chemistry";

const router = createHashRouter([
  {
    path: "/",
    element: <Content />,
  },
  {
    path: "chemistry",
    element: <Chemistry />,
  },
]);

function App() {
  return <RouterProvider router={router} />;
}

export default App;
