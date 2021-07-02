import * as React from "react";
import { graphql } from "gatsby";
import Layout from "../components/Layout";

const ShapePage = ({ data }) => {
  return (
    <Layout>
      <div>Shapes</div>
    </Layout>
  );
};

export default ShapePage;

export const pageQuery = graphql`
  query ShapePageQuery {
    allShape {
      nodes {
        name
        id
      }
    }
  }
`;
