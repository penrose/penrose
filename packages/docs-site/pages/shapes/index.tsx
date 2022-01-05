import dummy from "./mock-shapedata.json";
import Link from "next/link";

export default function ShapesPage({ shapes }) {
  return (
    <div>
      <h1>Shapes</h1>
      <ul>
        {Object.keys(shapes).map((shapeName) => (
          <li key={shapeName}>
            <Link href={"/shapes/" + shapeName}>{shapeName}</Link>
          </li>
        ))}
      </ul>
    </div>
  );
}

export function getStaticProps() {
  return {
    props: {
      shapes: dummy,
    },
  };
}
