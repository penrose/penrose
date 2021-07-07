import React from "react";
import { graphql } from "gatsby";
import Layout from "../components/Layout";
import {
  EuiPageContent,
  EuiPageContentBody,
  EuiPageHeader,
  EuiBasicTable,
} from "@elastic/eui";

const ShapePage = ({ data }) => {
  return (
    <Layout itemId={data.shape.id} title={data.shape.name}>
      <EuiPageHeader
        restrictWidth={800}
        pageTitle={data.shape.name}
        description="Shape definition"
      />
      <EuiPageContent>
        <EuiPageContentBody restrictWidth={800}>
          <EuiBasicTable
            columns={[
              { name: "Property", field: "name" },
              { name: "Type", field: "propType" },
            ]}
            items={data.shape.properties}
          />
        </EuiPageContentBody>
      </EuiPageContent>
    </Layout>
  );
};

export default ShapePage;

export const pageQuery = graphql`
  query($id: String) {
    shape(id: { eq: $id }) {
      id
      name
      properties {
        name
        propType
      }
    }
  }
`;
