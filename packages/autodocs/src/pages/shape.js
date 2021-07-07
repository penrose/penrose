import * as React from "react";
import { graphql, Link, navigate } from "gatsby";
import {
  EuiPageHeader,
  EuiCard,
  EuiFlexGroup,
  EuiFlexItem,
  EuiIcon,
  EuiPanel,
} from "@elastic/eui";
import Layout from "../components/Layout";

const ShapePage = ({ data }) => {
  return (
    <Layout title={"All Shapes"}>
      <EuiPageHeader pageTitle="Shapes" />
      <EuiPanel hasShadow={false}>
        <EuiFlexGroup gutterSize="l" wrap={true}>
          {data.allShape.nodes.map((shape) => (
            <EuiFlexItem key={`shape-${shape.name}`} grow={false}>
              <Link to={`/shape/${shape.name}`}>
                <EuiCard
                  layout="horizontal"
                  title={shape.name}
                  description="Shape definition"
                  icon={<EuiIcon size="xxl" type="heatmap" />}
                />
              </Link>
            </EuiFlexItem>
          ))}
        </EuiFlexGroup>
      </EuiPanel>
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
