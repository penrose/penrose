import tailwindTypography from "@tailwindcss/typography";
import tailwind from "tailwindcss";

export default {
  plugins: [
    tailwind({
      content: ["./.vitepress/theme/**/*.vue"],
      plugins: [tailwindTypography],
    }),
  ],
};
