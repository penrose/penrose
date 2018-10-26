import * as React from "react";
import { storiesOf } from "@storybook/react";
import { action } from "@storybook/addon-actions";

storiesOf("Button", module).add("with text", () => (
  <p onClick={action("clicked")}>test</p>
));
