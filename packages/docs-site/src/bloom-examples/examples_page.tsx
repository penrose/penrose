import EigenvectorsDiagram from "./eigen.js";
import Ender from "./ender_parallel_proof.js";

export default function () {
  return (
    <>
      <h1>Examples (WIP)</h1>
      <p>This page contains a list example diagrams built with Bloom.</p>

      <h3> Eigenvectors and Eigenspaces </h3>
      <EigenvectorsDiagram />

      <br />
      <h3> Geometric Explorations </h3>
      <Ender />
    </>
  );
}
