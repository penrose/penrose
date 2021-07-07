import React, { useState } from "react";
import { useStaticQuery, graphql, Link } from "gatsby";
import { Helmet } from "react-helmet";
import {
  EuiPage,
  EuiPageSideBar,
  EuiPageBody,
  EuiSideNav,
  EuiHeader,
  EuiTitle,
} from "@elastic/eui";
import "@elastic/eui/dist/eui_theme_amsterdam_light.css";

const Layout = ({ children, itemId, title }) => {
  const { allShape } = useStaticQuery(graphql`
    query SidebarQuery {
      allShape {
        nodes {
          name
          id
        }
      }
    }
  `);
  const sideItems = allShape.nodes.map((item) => ({
    ...item,
    isSelected: itemId === item.id,
    renderItem: ({ children }) => {
      return (
        <Link
          to={`/shape/${item.name}`}
          className="euiSideNavItemButton euiSideNavItemButton--isClickable"
          activeClassName="euiSideNavItemButton-isSelected"
        >
          {children}
        </Link>
      );
    },
  }));
  const [isSideNavOpenOnMobile, setisSideNavOpenOnMobile] = useState(false);

  const toggleOpenOnMobile = () => {
    setisSideNavOpenOnMobile((val) => !val);
  };
  return (
    <>
      <Helmet title={title ? `${title} - Penrose` : "Penrose Docs"} />
      <EuiHeader
        position="fixed"
        sections={[
          {
            items: [
              <Link to={"/"}>
                <EuiTitle size={"s"}>
                  <h1>Penrose</h1>
                </EuiTitle>
              </Link>,
            ],
          },
        ]}
      />
      <EuiPage paddingSize="s" style={{ marginTop: "48px" }}>
        <EuiPageSideBar paddingSize="m" sticky>
          <EuiSideNav
            toggleOpenOnMobile={toggleOpenOnMobile}
            isOpenOnMobile={isSideNavOpenOnMobile}
            mobileTitle="Menu"
            items={[
              {
                id: "shapes",
                name: "Shapes",
                items: sideItems,
                renderItem: ({ children }) => {
                  return (
                    <Link
                      to={`/shape`}
                      className="euiSideNavItemButton euiSideNavItemButton--isClickable"
                      activeClassName="euiSideNavItemButton-isSelected"
                    >
                      {children}
                    </Link>
                  );
                },
              },
            ]}
          />
        </EuiPageSideBar>
        <EuiPageBody panelled>{children}</EuiPageBody>
      </EuiPage>
    </>
  );
};

export default Layout;
