import dummy from "./mock-shapedata.json";
import Link from "next/link";

// TODO: render example style program with the shape - synthesize it
// maybe have editable text box for each prop?

export default function ShapePage({ shapeName, properties }) {
  return (
    <div>
      <Link href="/shapes">
        <h3>{"<"} all shapes</h3>
      </Link>
      <h1>{shapeName}</h1>
      <table>
        <tr>
          <th>Property</th>
          <th>Type</th>
        </tr>
        {Object.entries(properties).map(([prop, type]) => (
          <tr key={prop}>
            <td>{prop}</td>
            <td>{type}</td>
          </tr>
        ))}
      </table>
    </div>
  );
}

export function getStaticPaths() {
  const paths = Object.entries(dummy).map(([key, shape]) => ({
    params: {
      shape: key,
    },
  }));
  return { paths, fallback: false };
}

export function getStaticProps({ params }) {
  return {
    props: { shapeName: params.shape, properties: dummy[params.shape] },
  };
}
